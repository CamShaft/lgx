-module(lgx_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct.
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).

%% lgx
-export([series/1]).
-export([flat_parallel/1]).
-export([mixed/1]).
-export([conditional_true/1]).
-export([conditional_false/1]).
-export([nested_conditionals/1]).
-export([async/1]).
-export([deep_exec/1]).
-export([basic_memoize/1]).
-export([complex_memoize/1]).
-export([spawn_worker/1]).
-export([binary_tree/1]).
-export([variables/1]).

%% ct.

all() ->
  [series,
   flat_parallel,
   mixed,
   conditional_true,
   conditional_false,
   nested_conditionals,
   async,
   deep_exec,
   basic_memoize,
   complex_memoize,
   spawn_worker,
   binary_tree,
   variables].

init_per_suite(Config) ->
  Config.

end_per_suite(_) ->
  ok.

%% lgx.

%% P1
%% * mod:fun4(1)
%% P2
%% * mod:fun3(value(4))
%% P3
%% * mod:fun2(value(3))
%% P4
%% * mod:fun1(value(2))
series(_) ->
  {ok, AST} = lgx:compile([
    {1, mod, fun1, [{'$exec', 2}]},
    {2, mod, fun2, [{'$exec', 3}]},
    {3, mod, fun3, [{'$exec', 4}]},
    {4, mod, fun4, [1]}
  ], 1),
  Context = [],
  {ok, 5} = lgx:execute(AST, fun
    (mod, _, [Value], _Context, _Sender, _Ref) ->
      {ok, Value + 1}
  end, Context),
  ok.

%% P1
%% * mod:fun2(2)
%% * mod:fun3(3)
%% * mod:fun4(4)
%% P2
%% * mod:fun1(value(2), value(3), value(4))
flat_parallel(_) ->
  {ok, AST} = lgx:compile([
    {1, mod, fun1, [{'$exec', 2}, {'$exec', 3}, {'$exec', 4}]},
    {2, mod, fun2, [2]},
    {3, mod, fun3, [3]},
    {4, mod, fun4, [4]}
  ], 1),
  Context = [],
  {ok, 9} = lgx:execute(AST, fun
    (mod, fun1, [Val2, Val3, Val4], _Context, _Sender, _Ref) ->
      {ok, Val2 + Val3 + Val4};
    (mod, _, [Val], _Context, _Sender, _Ref) ->
      {ok, Val}
  end, Context),
  ok.

conditional_true(_) ->
  {ok, AST} = lgx:compile([
    {1, mod, fun1, [{'$exec', cond1}]},
    {cond1, '$cond', 2, 3, 4},
    {2, mod, fun2, [true]},
    {3, mod, fun3, []},
    {4, mod, fun4, []}
  ], 1),
  Context = [],
  {ok, 123} = lgx:execute(AST, fun
    (mod, fun1, [Result], _Context, _Sender, _Ref) ->
      {ok, Result};
    (mod, fun2, [Result], _Context, _Sender, _Ref) ->
      {ok, Result};
    (mod, fun3, [], _Context, _Sender, _Ref) ->
      {ok, 123};
    (mod, fun4, [], _Context, _Sender, _Ref) ->
      erlang:error({fun4_called})
  end, Context),
  ok.

conditional_false(_) ->
  {ok, AST} = lgx:compile([
    {1, mod, fun1, [{'$exec', cond1}]},
    {cond1, '$cond', 2, 3, 4},
    {2, mod, fun2, [false]},
    {3, mod, fun3, []},
    {4, mod, fun4, []}
  ], 1),
  Context = [],
  {ok, 123} = lgx:execute(AST, fun
    (mod, fun1, [Result], _Context, _Sender, _Ref) ->
      {ok, Result};
    (mod, fun2, [Result], _Context, _Sender, _Ref) ->
      {ok, Result};
    (mod, fun3, [], _Context, _Sender, _Ref) ->
      erlang:error({fun3_called});
    (mod, fun4, [], _Context, _Sender, _Ref) ->
      {ok, 123}
  end, Context),
  ok.

%% 1(
%%  cond(
%%    cond(
%%      cond(2(), 7(), 8()),
%%        5(), 6())
%%     3(), 4()),
%% cond(7(), 4(), 6()))
nested_conditionals(_) ->
  {ok, AST} = lgx:compile([
    {1, mod, '1', [{'$exec', cond1}, {'$exec', cond2}]},
    {cond1, '$cond', cond3, 3, 4},
    {cond2, '$cond', 7, 4, 6},
    {cond3, '$cond', cond4, 5, 6},
    {cond4, '$cond', 2, 7, 8},
    {2, mod, '2', []},
    {3, mod, '3', []},
    {4, mod, '4', []},
    {5, mod, '5', []},
    {6, mod, '6', []},
    {7, mod, '7', []},
    {8, mod, '8', []}
  ], 1),
  Context = [],
  {ok, {'4', '4'}} = lgx:execute(AST, fun
    (mod, '1', [Result1, Result2], _Context, _Sender, _Ref) ->
      {ok, {Result1, Result2}};
    (mod, '7', [], _Context, _Sender, _Ref) ->
      {ok, true};
    (mod, Num, [], _Context, _Sender, _Ref) ->
      {ok, Num}
  end, Context),
  ok.

%% P1
%% * mod:fun4(4)
%% P2
%% * mod:fun3(3, value(4))
%% * mod:fun2(2, value(4))
%% P3
%% * mod:fun1(1, value(2), value(3), value(4))
mixed(_) ->
  {ok, AST} = lgx:compile([
    {1, mod, fun1, [1, {'$exec', 2}, {'$exec', 3}, {'$exec', 4}]},
    {2, mod, fun2, [2, {'$exec', 4}]},
    {3, mod, fun3, [3, {'$exec', 4}]},
    {4, mod, fun4, [4]}
  ], 1),
  Context = [],
  {ok, 18} = lgx:execute(AST, fun
    (mod, fun1, [Val1, Val2, Val3, Val4], _Context, _Sender, _Ref) ->
      {ok, Val1 + Val2 + Val3 + Val4};
    (mod, fun2, [Val2,  Val4], _Context, _Sender, _Ref) ->
      {ok, Val2 + Val4};
    (mod, fun3, [Val3, Val4], _Context, _Sender, _Ref) ->
      {ok, Val3 + Val4};
    (mod, fun4, [Val], _Context, _Sender, _Ref) ->
      {ok, Val}
  end, Context),
  ok.

%% P1
%% * mod:fun2(2)
%% * mod:fun2(3)
%% * mod:fun2(4)
%% * mod:fun2(5)
%% * mod:fun2(6)
%% P2
%% * mod:fun1(1, {value(2), {value(3), [value(4), {value(5), value(6)}]}})
deep_exec(_) ->
  {ok, AST} = lgx:compile([
    {1, mod, fun1, [{1, {{'$exec', 2}, {{'$exec', 3}, [{'$exec', 4}, {{'$exec', 5}, {'$exec', 6}}]}}}]},
    {2, mod, fun2, [2]},
    {3, mod, fun2, [3]},
    {4, mod, fun2, [4]},
    {5, mod, fun2, [5]},
    {6, mod, fun2, [6]}
  ], 1),
  Context = [],
  {ok, {1, 2, 3, 4, 5, 6}} = lgx:execute(AST, fun
    (mod, fun1, [{V1, {V2, {V3, [V4, {V5, V6}]}}}], _Context, _Sender, _Ref) ->
      {ok, {V1, V2, V3, V4, V5, V6}};
    (mod, fun2, [Val], _Context, _Sender, _Ref) ->
      {ok, Val}
  end, Context),
  ok.

%% P1
%% * mod:fun4(1)
%% P2
%% * mod:fun3(value(4))
%% P3
%% * mod:fun2(value(3))
%% P4
%% * mod:fun1(value(2))
async(_) ->
  {ok, AST} = lgx:compile([
    {1, mod, fun1, [{'$exec', 2}]},
    {2, mod, fun2, [{'$exec', 3}]},
    {3, mod, fun3, [{'$exec', 4}]},
    {4, mod, fun4, [1]}
  ], 1),
  Context = [],
  {ok, 5} = lgx:execute(AST, fun
    (mod, _, [Value], _Context, Sender, ReqID) ->
      erlang:send_after(100, Sender, {ok, Value + 1, ReqID}),
      pending
  end, Context),
  ok.

%% P1
%% * mod:fun2(1)
%% * mod:fun2(1)
%% P2
%% * mod:fun1(value(2), value(3))
basic_memoize(_) ->
  {ok, AST} = lgx:compile([
    {1, mod, fun1, [{'$exec', 2}, {'$exec', 3}]},
    {2, mod, fun2, [1]},
    {3, mod, fun2, [1]}
  ], 1),
  Context = [],

  SendAfter = 100,
  %% Time < SendAfter * 2
  {ok, 4} = lgx:execute(AST, fun
    (mod, fun1, [Value, Value], _Context, _Sender, _Ref) ->
      {ok, Value + Value};
    (mod, fun2, [Value], _Context, Sender, ReqID) ->
      erlang:send_after(SendAfter, Sender, {ok, Value + 1, ReqID}),
      pending
  end, Context),
  ok.

%% P1a
%% * mod:fun3(1)
%% P1b
%% * mod:fun4(1)
%% P2a
%% * mod:fun2(value(4))
%% P2b
%% * mod:fun2(value(5))
%% P3
%% * mod:fun1(value(2), value(3))
complex_memoize(_) ->
  {ok, AST} = lgx:compile([
    {1, mod, fun1, [{'$exec', 2}, {'$exec', 3}]},
    {2, mod, fun2, [{'$exec', 4}]},
    {3, mod, fun2, [{'$exec', 5}]},
    {4, mod, fun3, [1]},
    {5, mod, fun4, [1]}
  ], 1),
  Context = [],

  SendAfter = 100,
  %% Time < SendAfter * 4
  {ok, 6} = lgx:execute(AST, fun
    (mod, fun1, [Value, Value], _Context, _Sender, _Ref) ->
      {ok, Value + Value};
    (mod, fun2, [Value], _Context, Sender, ReqID) ->
      erlang:send_after(SendAfter, Sender, {ok, Value + 1, ReqID}),
      pending;
    (mod, fun3, [Value], _Context, Sender, ReqID) ->
      erlang:send_after(SendAfter, Sender, {ok, Value + 1, ReqID}),
      pending;
    (mod, fun4, [Value], _Context, Sender, ReqID) ->
      erlang:send_after(SendAfter, Sender, {ok, Value + 1, ReqID}),
      pending
  end, Context),
  ok.

%% P1
%% * spawn(mod, fun2, [2])
%% * spawn(mod, fun3, [3])
%% * spawn(mod, fun4, [4])
%% P2
%% * mod:fun1(value(2), value(3), value(4))
spawn_worker(_) ->
  {ok, AST} = lgx:compile([
    {1, mod, fun1, [{'$exec', 2}, {'$exec', 3}, {'$exec', 4}]},
    {2, mod, fun2, [2], spawn},
    {3, mod, fun3, [3], spawn},
    {4, mod, fun4, [4], spawn}
  ], 1),
  Context = [],

  SleepTime = 100,
  %% Time < SleepTime * 4
  {ok, 9} = lgx:execute(AST, fun
    (mod, fun1, [Val2, Val3, Val4], _Context, _Sender, _Ref) ->
      {ok, Val2 + Val3 + Val4};
    (mod, _, [Val], _Context, _Sender, _Ref) ->
      timer:sleep(SleepTime),
      {ok, Val}
  end, Context),
  ok.

%% P1a
%% * mod:fun15(1)
%% * mod:fun14(1)
%% P1b
%% * mod:fun12(1)
%% * mod:fun11(1)
%% P1c
%% * mod:fun8(1)
%% * mod:fun7(1)
%% P1d
%% * mod:fun4(1)
%% * mod:fun5(1)
%% P2a
%% * mod:fun13(value(14), value(15))
%% P2b
%% * mod:fun10(value(11), value(11))
%% P2c
%% * mod:fun6(value(7), value(8))
%% P2d
%% * mod:fun3(value(4), value(5))
%% P3a
%% * mod:fun9(value(10), value(13))
%% P3b
%% * mod:fun2(value(3), value(6))
%% P4
%% * mod:fun1(value(2), value(9))
binary_tree(_) ->
  {ok, AST} = lgx:compile([
    {1, mod, fun1, [{'$exec', 2}, {'$exec', 9}]},
    {2, mod, fun2, [{'$exec', 3}, {'$exec', 6}]},
    {3, mod, fun3, [{'$exec', 4}, {'$exec', 5}]},
    {4, mod, fun4, [1]},
    {5, mod, fun5, [1]},
    {6, mod, fun6, [{'$exec', 7}, {'$exec', 8}]},
    {7, mod, fun7, [1]},
    {8, mod, fun8, [1]},
    {9, mod, fun9, [{'$exec', 10}, {'$exec', 13}]},
    {10, mod, fun10, [{'$exec', 11}, {'$exec', 12}]},
    {11, mod, fun11, [1]},
    {12, mod, fun12, [1]},
    {13, mod, fun13, [{'$exec', 14}, {'$exec', 15}]},
    {14, mod, fun14, [1]},
    {15, mod, fun15, [1]}
  ], 1),
  Context = [],

  {ok, 15} = lgx:execute(AST, fun
    (mod, _, [Value1, Value2], _Context, _Sender, _Ref) ->
      {ok, Value1 + Value2 + 1};
    (mod, _, [Value], _Context, _Sender, _Ref) ->
      {ok, Value}
  end, Context),
  ok.

variables(_) ->
  {ok, AST} = lgx:compile([
    {1, '$var', 2},
    {2, '$var', 3},
    {3, '$var', 4},
    {4, '$var', 5},
    {5, mod, fun1, []}
  ], 1),
  Context = [],
  {ok, 1} = lgx:execute(AST, fun
    (_, _, _, _, _, _) ->
      {ok, 1}
  end, Context),
  ok.
