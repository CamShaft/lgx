-module(expr_test).

-include_lib("eunit/include/eunit.hrl").

expr_test_() ->
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
