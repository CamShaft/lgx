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

-define(TEST6, {
  #state{
     vars = #{
       2 => #expr{id = 2, type = call, value = {mod, fn}, children = []}
     },
     pending = [
       #expr{id = 1, type = call, value = {mod, fn}, is_root = true, children = [
         #expr{type = call, value = {mod, fn}, children = [
           #expr{type = call, value = {mod, fn}, children = []},
           #expr{type = call, value = {mod, fn}, children = []},
           2
         ]},
         #expr{type = call, value = {mod, fn}, children = [
           #expr{type = call, value = {mod, fn}, children = []},
           2,
           #expr{type = call, value = {mod, fn}, children = []}
         ]},
         #expr{type = call, value = {mod, fn}, children = [
           2,
           #expr{type = call, value = {mod, fn}, children = []},
           #expr{type = call, value = {mod, fn}, children = []}
         ]}
       ]}
     ],
     counter = 2
  },
  ?NOOP,
  {},
  [[[], [], []],[[], [], []],[[], [], []]]
}).

-define(TEST7, {
  #state{
     vars = #{
       4 => #expr{id = 4, type = call, value = {mod4, fn}, children = []},
       2 => #expr{id = 2, type = call, value = {mod2, fn}, children = [4, 4, 4]}
     },
     pending = [
       #expr{id = 1, type = call, value = {mod1, fn}, is_root = true, children = [
         #expr{type = call, value = {mod1_1, fn}, children = [
           #expr{type = call, value = {mod1_1_1, fn}, children = []},
           #expr{type = call, value = {mod1_1_2, fn}, children = []},
           2
         ]},
         #expr{type = call, value = {mod1_2, fn}, children = [
           #expr{type = call, value = {mod1_2_2, fn}, children = []},
           2,
           #expr{type = call, value = {mod1_2_3, fn}, children = []}
         ]},
         #expr{type = call, value = {mod1_3, fn}, children = [
           2,
           #expr{type = call, value = {mod1_3_2, fn}, children = []},
           #expr{type = call, value = {mod1_3_3, fn}, children = []}
         ]}
       ]}
     ],
     counter = 3
  },
  fun(Module, _Fun, Args, _, _, _) ->
    {ok, [Module|Args]}
  end,
  {},
  [mod1,
    [mod1_1,
      [mod1_1_1],
      [mod1_1_2],
      [mod2,[mod4],[mod4],[mod4]]],
    [mod1_2,
      [mod1_2_2],
      [mod2,[mod4],[mod4],[mod4]],
      [mod1_2_3]],
    [mod1_3,
      [mod2,[mod4],[mod4],[mod4]],
      [mod1_3_2],
      [mod1_3_3]]]
}).

-define(TEST8, {
  #state{
     pending = [
       #expr{id = 1, type = call, value = {mod, fn}, is_root = true, children = [
         #expr{type = literal, value = <<"IT WORKED!!!">>},
         #expr{type = literal, value = <<"Yes it did.">>}
       ]}
     ],
     counter = 1
  },
  ?NOOP,
  {},
  [<<"IT WORKED!!!">>,<<"Yes it did.">>]
}).

-define(TEST9, {
  #state{
     vars = #{
       4 => #expr{id = 4, type = literal, value = bar},
       2 => #expr{id = 2, type = literal, value = foo}
     },
     pending = [
       #expr{id = 1, type = call, value = {mod, fn}, is_root = true, children = [2, 4]}
     ],
     counter = 3
  },
  ?NOOP,
  {},
  [foo, bar]
}).

-define(TESTS, [
  ?TEST1,
  ?TEST2,
  ?TEST3,
  ?TEST4,
  ?TEST5,
  ?TEST6,
  ?TEST7,
  ?TEST8,
  ?TEST9
]).

runtime_test_() ->
  [fun() ->
     {ok, Res, _State} = expr_runtime:execute(State, Fun, Context),
     ?assertEqual(Expected, Res)
  end || {State, Fun, Context, Expected} <- ?TESTS].
