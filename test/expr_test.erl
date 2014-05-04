-module(expr_test).

-include_lib("eunit/include/eunit.hrl").
-include("../src/expr.hrl").

-define(WILDCARD(Type), (begin
  {ok, CWD} = file:get_cwd(),
  Folder = case filename:basename(CWD) of
    ".eunit" -> filename:join(CWD, "../test/data");
    _ -> filename:join(CWD, "test/data")
  end,
  Tests = filelib:wildcard(Folder ++ "/" ++ Type ++ ".*.ast"),
  [filename:join(Folder, filename:basename(Test, ".ast")) || Test <- Tests]
end)).

literal_test_() ->
  [fun() -> expr_apply(Test) end || Test <- ?WILDCARD("literal")].

compound_test_() ->
  [fun() -> expr_apply(Test) end || Test <- ?WILDCARD("compound")].

call_test_() ->
  [fun() -> expr_apply(Test) end || Test <- ?WILDCARD("call")].

comprehension_test_() ->
  [fun() -> expr_apply(Test) end || Test <- ?WILDCARD("comprehension")].

conditional_test_() ->
  [fun() -> expr_apply(Test) end || Test <- ?WILDCARD("conditional")].

assign_test_() ->
  [fun() -> expr_apply(Test) end || Test <- ?WILDCARD("assign")].

error_test_() ->
  [fun() -> expr_error(Test) end || Test <- ?WILDCARD("error")].

expr_apply(Test) ->
  {ok, Forms} = file:consult(Test ++ ".ast"),

  {ok, Context} = case file:consult(Test ++ ".context") of
    {ok, [C]} ->
      {ok, C};
    {error, enoent} ->
      {ok, []};
    Error ->
      Error
  end,

  {ok, [Expected]} = file:consult(Test ++ ".out"),

  {ok, Actual, _State} = expr:apply(Forms, fun resolve/7, Context),
  ?assertEqual(Expected, Actual).

expr_error(Test) ->
  {ok, Forms} = file:consult(Test ++ ".ast"),

  {ok, Context} = case file:consult(Test ++ ".context") of
    {ok, [C]} ->
      {ok, C};
    {error, enoent} ->
      {ok, []};
    Error ->
      Error
  end,

  {error, _Error, _State} = expr:apply(Forms, fun resolve/7, Context).

resolve(_Mod, error, [Error], _Context, _Sender, _Ref, _Attrs) ->
  {error, Error};
resolve(_Mod, cache, [Arg], 0, _Sender, _Ref, _Attrs) ->
  {ok, Arg, 1};
resolve(_Mod, cache, _, _Count, _Sender, _Ref, _Attrs) ->
  {error, did_not_cache};
resolve(Mod, Fn, Args, _Context, _Sender, _Ref, _Attrs) ->
  {ok, {Mod, Fn, Args}}.
