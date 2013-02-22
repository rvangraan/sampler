%%%-------------------------------------------------------------------
%%% File    : sampler_SUITE.erl
%%% Author  : Rudolph van Graan <>
%%% Description : 
%%%
%%% Created :  4 Feb 2007 by Rudolph van Graan <>
%%%-------------------------------------------------------------------
-module(sampler_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("test_server/include/test_server.hrl").

%%--------------------------------------------------------------------
init_per_suite(Config) ->
  Config.

%%--------------------------------------------------------------------
end_per_suite(_Config) ->
  ok.

%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
  Config.

%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
  ok.

%%--------------------------------------------------------------------
all(doc) -> 
  ["Describe the main purpose of this suite"];

all(suite) -> 
  [sample_absolute,
   sample_counter,
   auto_sample].


counter_proc_start(OwnerPid,Value) ->
  MyPid = self(),
  erlang:monitor(process,OwnerPid),
  register(counter_proc,MyPid),
  
  counter_proc(OwnerPid,Value).

counter_proc(OwnerPid,Value) ->
  receive
    {get_value,Pid} ->
      Pid ! {ok,Value},
      counter_proc(OwnerPid,Value);
    {set_value,Pid,NewValue} ->
      counter_proc(OwnerPid,NewValue);
    {'DOWN',_Ref,process,OwnerPid,_Reason} ->
      normal
  end.

set_value(Value) ->
  counter_proc ! {set_value,self(),Value},
  ok.


get_counter_value([]) ->
  counter_proc ! {get_value,self()},
  receive
    {ok,Value} ->
      Value
  after
    1000 ->
      throw({timeout,retrieving_value})
  end.


%% Test cases starts here.
%%--------------------------------------------------------------------
sample_absolute(doc) -> 
  ["Tests manual sampling"];
sample_absolute(suite) -> 
  [];
sample_absolute(Config) when is_list(Config) ->
  ?line LPid = proc_lib:spawn_link(?MODULE,counter_proc_start,[self(),0]),
  %% Start the sampler with this as the sampler module

  ?line {ok,PID} = sampler:start_link(absolute,manual_sampling,4,?MODULE,get_counter_value,[]),  
  ?line 0 = sampler:queue_size(PID),
  ?line 0 = sampler:get_moving_average(PID),

  ?line ok = set_value(10),  
  ?line ok = sampler:sample(PID),
  ?line 1 = sampler:queue_size(PID),
  ?line 10 = round(sampler:get_moving_average(PID)),

  ?line ok = set_value(10),    
  ?line ok = sampler:sample(PID),
  ?line 2 = sampler:queue_size(PID),
  ?line 10 = round(sampler:get_moving_average(PID)),
  
  ?line ok = set_value(1),    
  ?line ok = sampler:sample(PID),
  ?line 3 = sampler:queue_size(PID),
  ?line 7 = round(sampler:get_moving_average(PID)),
  
  ?line ok = set_value(3),    
  ?line ok = sampler:sample(PID),
  ?line 4 = sampler:queue_size(PID),
  ?line 6 = round(sampler:get_moving_average(PID)),
  
  ?line ok = set_value(2),    
  ?line ok = sampler:sample(PID),
  ?line 4 = sampler:queue_size(PID),
  ?line 4 = round(sampler:get_moving_average(PID)),
  
  ok.

sample_counter(doc) -> 
  ["Tests counter sampling"];
sample_counter(suite) -> 
  [];
sample_counter(Config) when is_list(Config) ->
  ?line LPid = proc_lib:spawn_link(?MODULE,counter_proc_start,[self(),0]),
  %% Start the sampler with this as the sampler module

  ?line {ok,PID} = sampler:start_link(counter,manual_sampling,4,?MODULE,get_counter_value,[]),  
  ?line 0 = sampler:queue_size(PID),
  ?line 0 = sampler:get_moving_average(PID),
  ?line 0 = sampler:get_sum(PID),

  %% First reading on counters is zero - due to us having to take a diff
  ?line ok = set_value(10),  
  ?line ok = sampler:sample(PID),
  ?line 0 = sampler:queue_size(PID),
  ?line 0 = round(sampler:get_moving_average(PID)),
  ?line 0 = sampler:get_sum(PID),
  
  ?line ok = set_value(20),  
  ?line ok = sampler:sample(PID),
  ?line 1 = sampler:queue_size(PID),
  ?line 10 = round(sampler:get_moving_average(PID)),
  ?line 10 = sampler:get_sum(PID),

  ?line ok = set_value(20),  
  ?line ok = sampler:sample(PID),
  ?line 2 = sampler:queue_size(PID),
  ?line 5 = round(sampler:get_moving_average(PID)),
  ?line 10 = sampler:get_sum(PID),

  ?line ok = set_value(30),  
  ?line ok = sampler:sample(PID),
  ?line 3 = sampler:queue_size(PID),
  ?line 7 = round(sampler:get_moving_average(PID)),
  ?line 20 = sampler:get_sum(PID),
  ok.


auto_sample(doc) -> 
  ["Tests auto sampling"];
auto_sample(suite) -> 
  [];
auto_sample(Config) when is_list(Config) ->
  ?line LPid = proc_lib:spawn_link(?MODULE,counter_proc_start,[self(),0]),
  %% Start the sampler with this as the sampler module
  ?line {ok,PID} = sampler:start_link(counter,100,4,?MODULE,get_counter_value,[]),  
  ?line ok = set_value(20),  

  ?line 0 = sampler:samples(PID),
  ?line ok = timer:sleep(120),
  ?line 1 = sampler:samples(PID),
  ?line ok = timer:sleep(120),
  ?line 2 = sampler:samples(PID),
  ?line 0 = round(sampler:get_moving_average(PID)).
