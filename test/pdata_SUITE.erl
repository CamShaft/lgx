-module(pdata_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct.
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).

%% pdata
-export([series/1]).
-export([flat_parallel/1]).
-export([mixed/1]).
-export([async/1]).
-export([deep_exec/1]).
-export([basic_memoize/1]).
-export([complex_memoize/1]).
-export([spawn_worker/1]).

%% ct.

all() ->
  [series, flat_parallel, mixed, async, deep_exec, basic_memoize, complex_memoize, spawn_worker].

init_per_suite(Config) ->
  Config.

end_per_suite(_) ->
  ok.

%% pdata.

series(_) ->
  Graph = pdata:compile([
    {1, mod, fun1, [{'$exec', 2}]},
    {2, mod, fun2, [{'$exec', 3}]},
    {3, mod, fun3, [{'$exec', 4}]},
    {4, mod, fun4, [1]}
  ], 1),
  Context = [],
  {ok, 4} = pdata:execute(Graph, fun
    (mod, _, [Value], _Context, _Sender) ->
      {ok, Value + 1}
  end, Context),
  ok.

flat_parallel(_) ->
  Graph = pdata:compile([
    {1, mod, fun1, [{'$exec', 2}, {'$exec', 3}, {'$exec', 4}]},
    {2, mod, fun2, [2]},
    {3, mod, fun3, [3]},
    {4, mod, fun4, [4]}
  ], 1),
  Context = [],
  {ok, 9} = pdata:execute(Graph, fun
    (mod, fun1, [Val2, Val3, Val4], _Context, _Sender) ->
      {ok, Val2 + Val3 + Val4};
    (mod, _, [Val], _Context, _Sender) ->
      {ok, Val}
  end, Context),
  ok.

mixed(_) ->
  Graph = pdata:compile([
    {1, mod, fun1, [1, {'$exec', 2}, {'$exec', 3}, {'$exec', 4}]},
    {2, mod, fun2, [2, {'$exec', 3}, {'$exec', 4}]},
    {3, mod, fun3, [3, {'$exec', 4}]},
    {4, mod, fun4, [4]}
  ], 1),
  Context = [],
  {ok, 30} = pdata:execute(Graph, fun
    (mod, fun1, [Val1, Val2, Val3, Val4], _Context, _Sender) ->
      {ok, Val1 + Val2 + Val3 + Val4};
    (mod, fun2, [Val2, Val3, Val4], _Context, _Sender) ->
      {ok, Val2 + Val3 + Val4};
    (mod, fun3, [Val3, Val4], _Context, _Sender) ->
      {ok, Val3 + Val4};
    (mod, fun4, [Val], _Context, _Sender) ->
      {ok, Val}
  end, Context),
  ok.

deep_exec(_) ->
  Graph = pdata:compile([
    {1, mod, fun1, [{1, {{'$exec', 2}, {{'$exec', 3}, [{'$exec', 4}, {{'$exec', 5}, {'$exec', 6}}]}}}]},
    {2, mod, fun2, [2]},
    {3, mod, fun2, [3]},
    {4, mod, fun2, [4]},
    {5, mod, fun2, [5]},
    {6, mod, fun2, [6]}
  ], 1),
  Context = [],
  {ok, {1, 2, 3, 4, 5, 6}} = pdata:execute(Graph, fun
    (mod, fun1, [{V1, {V2, {V3, [V4, {V5, V6}]}}}], _Context, _Sender) ->
      {ok, {V1, V2, V3, V4, V5, V6}};
    (mod, fun2, [Val], _Context, _Sender) ->
      {ok, Val}
  end, Context),
  ok.

async(_) ->
  Graph = pdata:compile([
    {1, mod, fun1, [{'$exec', 2}]},
    {2, mod, fun2, [{'$exec', 3}]},
    {3, mod, fun3, [{'$exec', 4}]},
    {4, mod, fun4, [1]}
  ], 1),
  Context = [],
  {ok, 4} = pdata:execute(Graph, fun
    (mod, _, [Value], _Context, Sender) ->
      erlang:send_after(100, Sender, Value + 1),
      pending
  end, Context),
  ok.

basic_memoize(_) ->
  Graph = pdata:compile([
    {1, mod, fun1, [{'$exec', 2}, {'$exec', 3}]},
    {2, mod, fun2, [1]},
    {3, mod, fun2, [1]}
  ], 1),
  Context = [],

  SendAfter = 100,
  %% Time < SendAfter * 2
  {ok, 4} = pdata:execute(Graph, fun
    (mod, fun1, [Value, Value], _Context, _Sender) ->
      {ok, Value + Value};
    (mod, fun2, [Value], _Context, Sender) ->
      erlang:send_after(SendAfter, Sender, Value + 1),
      pending
  end, Context),
  ok.

complex_memoize(_) ->
  Graph = pdata:compile([
    {1, mod, fun1, [{'$exec', 2}, {'$exec', 3}]},
    {2, mod, fun2, [{'$exec', 4}]},
    {3, mod, fun2, [{'$exec', 5}]},
    {4, mod, fun3, [1]},
    {5, mod, fun4, [1]}
  ], 1),
  Context = [],

  SendAfter = 100,
  %% Time < SendAfter * 4
  {ok, 12} = pdata:execute(Graph, fun
    (mod, fun1, [Value, Value], _Context, _Sender) ->
      {ok, Value + Value};
    (mod, fun2, [Value], _Context, Sender) ->
      erlang:send_after(SendAfter, Sender, Value + 1),
      pending;
    (mod, fun3, [Value], _Context, Sender) ->
      erlang:send_after(SendAfter, Sender, Value + 1),
      pending;
    (mod, fun4, [Value], _Context, Sender) ->
      erlang:send_after(SendAfter, Sender, Value + 1),
      pending
  end, Context),
  ok.

spawn_worker(_) ->
  Graph = pdata:compile([
    {1, mod, fun1, [{'$exec', 2}, {'$exec', 3}, {'$exec', 4}]},
    {2, mod, fun2, [2], spawn},
    {3, mod, fun3, [3], spawn},
    {4, mod, fun4, [4], spawn}
  ], 1),
  Context = [],

  SleepTime = 100,
  %% Time < SleepTime * 4
  {ok, 9} = pdata:execute(Graph, fun
    (mod, fun1, [Val2, Val3, Val4], _Context, _Sender) ->
      {ok, Val2 + Val3 + Val4};
    (mod, _, [Val], _Context, _Sender) ->
      timer:sleep(SleepTime),
      {ok, Val}
  end, Context),
  ok.
