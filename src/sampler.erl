%%%-------------------------------------------------------------------
%%% File    :  sampler.erl
%%% Author  :  Rudolph van Graan <>
%%% Copyright: Rudolph van Graan
%%% Descriptionx
%%%
%%% Created :  4 Feb 2007 by Rudolph van Graan <>
%%%-------------------------------------------------------------------
-module(sampler).

-behaviour(gen_server).

%% API
-export([start_link/6,
	 queue_size/1,
	 get_moving_average/1,
	 get_sum/1,
	 sample/1,
	 samples/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {m,f,a,
		owner,
		sample,
		mode,
		length,
		timer,
		samples=0,
		last=undefined,
	        values=[]}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Mode = counter 
%% SampleInterval = integer() | manual_sampling
%% QueueLength    = integer()
%% M,F,Args
%%--------------------------------------------------------------------
start_link(Mode,SampleInterval,QueueLength,M,F,Args) ->
  PID = self(),
  gen_server:start_link(?MODULE, [PID,Mode,SampleInterval,QueueLength,M,F,Args], []).

queue_size(PID) ->
  gen_server:call(PID,queue_size).

get_moving_average(PID) ->
  gen_server:call(PID,get_moving_average).

get_sum(PID) ->
  gen_server:call(PID,get_sum).

sample(PID) ->
  gen_server:call(PID,sample).

samples(PID) ->
  gen_server:call(PID,samples).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
init([OwnerPID,Mode,SampleInterval,QueueLength,M,F,Args]) when SampleInterval == manual_sampling ->
  erlang:monitor(process,OwnerPID),
  {ok, #state{m=M,f=F,a=Args,
	      owner=OwnerPID,
	      mode=Mode,sample=SampleInterval,
	      length=QueueLength}};
init([OwnerPID,Mode,SampleInterval,QueueLength,M,F,Args]) when is_integer(SampleInterval),
						      SampleInterval > 0->
  erlang:monitor(process,OwnerPID),
  {ok,Timer} = timer:apply_interval(SampleInterval,?MODULE,sample,[self()]),
  {ok, #state{m=M,f=F,a=Args,
	      owner=OwnerPID,
	      timer = Timer,
	      mode=Mode,sample=SampleInterval,
	      length=QueueLength}}.

%%--------------------------------------------------------------------
handle_call(get_moving_average, _From, State) when State#state.values == [] ->
  Reply = 0,
  {reply, Reply, State};
handle_call(get_moving_average, _From, State) when State#state.values =/= [] ->
  S = lists:foldl(fun(V,Acc) -> V+Acc end,0, State#state.values),
  Reply = S/length(State#state.values),
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
  {reply, Reply, State#state{last=FirstValue,samples=State#state.samples+1}};
handle_call(sample, _From, State) when State#state.mode == counter,
                                       State#state.last =/= undefined->
  NextValue = get_value(State),
  LastValue = State#state.last,
  Reply = ok,
  NewValues = string:substr([(NextValue-LastValue)|State#state.values],1,State#state.length),
  {reply, Reply, State#state{last=NextValue,
			     values=NewValues,
			     samples=State#state.samples+1}}.
  

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
  M    = State#state.m,
  F    = State#state.f,
  Args = State#state.a,
  M:F(Args).
