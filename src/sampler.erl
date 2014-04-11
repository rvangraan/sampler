%%%-------------------------------------------------------------------
%%% File    :  sampler.erl
%%% Author  :  Rudolph van Graan <>
%%% Copyright: Rudolph van Graan
%%% Description
%%%
%%% Created :  4 Feb 2007 by Rudolph van Graan <>
%%%-------------------------------------------------------------------
-module(sampler).

-behaviour(gen_server).

-include("../include/sampler.hrl").
%% API
-export([start_link/1,
	 start_link/6,
	 queue_size/1,
	 get_moving_average/1,
	 get_sum/1,
	 sample/1,
	 samples/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {sample_func,
		name,
		owner,
		sample,
		mode,
		length,
		timer,
		scope = local,
		gproc_property,
		gproc_label,
		samples=0,
		last=undefined,
	        values=[]}).

%%====================================================================
%% API
%%====================================================================

start_link(Sampler = #sampler{}) ->
    PID = self(),
    gen_server:start_link(?MODULE, [PID,Sampler], []).
%%--------------------------------------------------------------------
start_link(Mode,SampleInterval,QueueLength,M,F,Args) ->
  PID = self(),
  gen_server:start_link(?MODULE, [PID,#sampler{mode            = Mode,
					       sample_interval = SampleInterval,
					       history_length  = QueueLength,
					       sample_func     = {M,F,Args}
					      }], []).
%%--------------------------------------------------------------------
queue_size(NameOrPid) ->
  gen_server:call(locate(NameOrPid),queue_size).

%%--------------------------------------------------------------------
get_moving_average(NameOrPid) ->
  gen_server:call(locate(NameOrPid),get_moving_average).

%%--------------------------------------------------------------------
get_sum(NameOrPid) ->
  gen_server:call(locate(NameOrPid),get_sum).

%%--------------------------------------------------------------------
sample(NameOrPid) ->
  gen_server:call(locate(NameOrPid),sample).

%%--------------------------------------------------------------------
samples(NameOrPid) ->
  gen_server:call(locate(NameOrPid),samples).


%%--------------------------------------------------------------------
locate(Pid) when is_pid(Pid) ->
    Pid;
locate(Name) ->
    gproc:where({n,l, {sampler, Name}}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
init([OwnerPID,Sampler = #sampler{sample_interval = SampleInterval}]) ->
    erlang:monitor(process,OwnerPID),
    ok = register_name(Sampler),
    {ok,Timer} = case SampleInterval of 
		     manual_sampling ->
			 {ok,undefined};
		     SamplerInterval when is_integer(SamplerInterval),
					  SamplerInterval > 0 ->
			 timer:apply_interval(SampleInterval,?MODULE,sample,[self()])
		 end,
    {ok, #state{sample_func    = ensure_fun(Sampler#sampler.sample_func),
		name           = Sampler#sampler.name,
		owner          = OwnerPID,
		timer          = Timer,
		mode           = Sampler#sampler.mode,
		scope          = Sampler#sampler.scope,
		sample         = Sampler#sampler.sample_interval,
		gproc_property = Sampler#sampler.gproc_property,
		gproc_label    = Sampler#sampler.gproc_label,
		length         = Sampler#sampler.history_length}}.
%%--------------------------------------------------------------------
ensure_fun({M,F,A}) ->
    fun() ->
	    erlang:apply(M,F,A)
    end;
ensure_fun(Fun) when is_function(Fun,0) ->
    Fun.
%%--------------------------------------------------------------------
register_name(#sampler{name = Name, 
		       gproc_property = GProcProperty,
		       scope = Scope} = Sampler) when Scope == local;
						      Scope == global ->
    GprocScope = case Scope of
		     local -> l;
		     global -> g
		 end,
    case Name of 
	undefined -> ok;
	Name      ->
	    true = gproc:reg({n,GprocScope, {sampler, Name}})
    end,
    case GProcProperty of
	undefined -> ok;
	GProcProperty ->
	    Label = Sampler#sampler.gproc_label,
	    true = gproc:reg({p,GprocScope, GProcProperty},{Label,0})
    end,
    true = gproc:reg({p,GprocScope, sample_interval},{Name,Sampler#sampler.sample_interval}),
    ok.


%%--------------------------------------------------------------------
handle_call(get_moving_average, _From, State) ->
  Reply = calculate_moving_average(State),
  {reply, Reply, State};

handle_call(get_sum, _From, State) when State#state.mode =:= counter ->
  {reply,lists:sum(State#state.values),State};

handle_call(queue_size, _From, State) ->
  Reply = length(State#state.values),
  {reply, Reply, State};
handle_call(samples, _From, State) ->
  Reply = State#state.samples,
  {reply, Reply, State};
handle_call(sample, _From, State) when State#state.mode == absolute ->
  NextValue = get_value(State),
  Reply = ok,
  NewValues = string:substr([NextValue|State#state.values],1,State#state.length),
  {reply, Reply, State#state{values=NewValues,samples=State#state.samples+1}};
handle_call(sample, _From, State) when State#state.mode == counter,
                                       State#state.last == undefined,
                                       State#state.values == []->
    FirstValue = get_value(State),
    Reply = ok,
    NewState = State#state{last=FirstValue,samples=State#state.samples+1},
    ok = publish_value(NewState),
    {reply, Reply, NewState};
handle_call(sample, _From, State) when State#state.mode == counter,
                                       State#state.last =/= undefined->
    NextValue = get_value(State),
    LastValue = State#state.last,
    Reply = ok,
    NewValues = string:substr([(NextValue-LastValue)|State#state.values],1,State#state.length),
    NewState = State#state{last=NextValue,
			   values=NewValues,
			   samples=State#state.samples+1},
    ok = publish_value(NewState),
    {reply, Reply, NewState}.
  
%%--------------------------------------------------------------------
publish_value(State) ->
    MovAvg = calculate_moving_average(State),
    Scope = case State#state.scope of
		local ->  l;
		global -> g
	    end,
    case State#state.gproc_property of
	undefined -> ok;
	GProcProperty ->
	    Label = State#state.gproc_label,
	    true = gproc:set_value({p,Scope,GProcProperty},{Label,MovAvg}),
	    ok
    end.
%%--------------------------------------------------------------------
calculate_moving_average(State) when State#state.values == [] ->
    0;
calculate_moving_average(State) when State#state.values =/= [] ->
    S = lists:foldl(fun(V,Acc) -> V+Acc end,0, State#state.values),
    S/length(State#state.values).
    

%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
handle_info({'DOWN',_Ref,process,OwnerPID,_Reason}, State) when OwnerPID == State#state.owner->
  {stop,normal,State}.

%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------


get_value(State) ->
    SampleFun    = State#state.sample_func,
    try
	case SampleFun() of
	    Integer when is_integer(Integer) ->
		Integer;
	    Else ->
		error_logger:warning_report([{sampler, State#state.name},
					     {message, "Sampler returned invalid value. Value should be an integer"},
					     {value, Else}]),
		0
	end
    catch
	C:E ->
	    error_logger:error_report([{sampler, State#state.name},
				       {message,"Unable to sample value. Value ignored."},
				       {reason,E},
				       {stacktrace,erlang:get_stacktrace()}]),
	    0
    end.
