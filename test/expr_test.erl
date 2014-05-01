-module(expr_test).

-include_lib("eunit/include/eunit.hrl").
-include("../src/expr.hrl").

%% expr_test_() ->
expr_test() ->
  {ok, CWD} = file:get_cwd(),
  Folder = filename:join(CWD, "test/data"),
  Tests = filelib:wildcard(Folder ++ "/*.ast"),
  [fun() -> expr(filename:join(Folder, filename:basename(Test, ".ast"))) end || Test <- Tests].

expr(Test) ->
  {ok, Forms} = file:consult(Test ++ ".ast"),

  {ok, AST} = expr:compile(Forms),

  {ok, Context} = case file:consult(Test ++ ".context") of
    {ok, [C]} ->
      {ok, C};
    {error, enoent} ->
      {ok, []};
    Error ->
      Error
  end,

  {ok, [Expected]} = file:consult(Test ++ ".out"),

  {ok, Actual} = expr:execute(AST, fun resolve/6, Context),
  ?assertEqual(Expected, Actual).

resolve(_Mod, _Fn, _Args, _Context, _Sender, _Ref) ->
  {ok, todo}.

%%% runtime

-define(NOOP, fun(Module, Fun, Args, _, _, _) ->
  io:format("~p:~p(~p)~n", [Module, Fun, Args]),
  {ok, Args}
end).

-define(TEST1, {
  #state{
    pending = [
      #expr{value = atom, is_root = true}
    ]
  },
  ?NOOP,
  {},
  atom
}).

-define(TEST2, {
  #state{
     values = #{
       1 => 1,
       2 => 2,
       4 => 3
     },
     completed = 7,
     pending = [
       #expr{id = 8, type = call, value = {mod, fn}, is_root = true, deps = 7, status = waiting, children = [1, 2, 4]}
     ]
  },
  ?NOOP,
  {},
  [1, 2, 3]
}).

-define(TEST3, {
  #state{
     pending = [
       #expr{id = 1, type = call, value = {mod, fn}, is_root = true, children = [
         #expr{type = call, value = {mod, fn}, children = []}
       ]}
     ],
     counter = 1
  },
  ?NOOP,
  {},
  [[]]
}).

-define(TEST4, {
  #state{
     pending = [
       #expr{id = 1, type = call, value = {mod, fn}, is_root = true, children = [
         #expr{type = call, value = {mod, fn}, children = []},
         #expr{type = call, value = {mod, fn}, children = []},
         #expr{type = call, value = {mod, fn}, children = []}
       ]}
     ],
     counter = 1
  },
  ?NOOP,
  {},
  [[], [], []]
}).

-define(TEST5, {
  #state{
     pending = [
       #expr{id = 1, type = call, value = {mod, fn}, is_root = true, children = [
         #expr{type = call, value = {mod, fn}, children = [
           #expr{type = call, value = {mod, fn}, children = []},
           #expr{type = call, value = {mod, fn}, children = []},
           #expr{type = call, value = {mod, fn}, children = []}
         ]},
         #expr{type = call, value = {mod, fn}, children = [
           #expr{type = call, value = {mod, fn}, children = []},
           #expr{type = call, value = {mod, fn}, children = []},
           #expr{type = call, value = {mod, fn}, children = []}
         ]},
         #expr{type = call, value = {mod, fn}, children = [
           #expr{type = call, value = {mod, fn}, children = []},
           #expr{type = call, value = {mod, fn}, children = []},
           #expr{type = call, value = {mod, fn}, children = []}
         ]}
       ]}
     ],
     counter = 1
  },
  ?NOOP,
  {},
  [[[], [], []],[[], [], []],[[], [], []]]
}).

-define(TESTS, [
  ?TEST1,
  ?TEST2,
  ?TEST3,
  ?TEST4,
  ?TEST5
]).

runtime_test_() ->
  [fun() ->
     {ok, Res, _State} = expr_runtime:execute(State, Fun, Context),
     ?assertEqual(Expected, Res)
  end || {State, Fun, Context, Expected} <- ?TESTS].
