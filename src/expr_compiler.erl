-module(expr_compiler).

-export([compile/1]).

-include("expr.hrl").

-define(PASSES, [
  fun to_records/2,
  fun mark_root/2,
  fun init_state/2
  %% fun extract_variables/2
]).

compile(Exprs) ->
  compile(Exprs, ?PASSES).

compile(Exprs, []) ->
  {ok, Exprs};
compile(Exprs, [Pass|Passes]) ->
  {ok, Exprs2} = Pass(Exprs, []),
  compile(Exprs2, Passes).

%% convert the maps to the internal records
to_records([], Acc) ->
  {ok, lists:reverse(Acc)};
to_records([Expr|Exprs], Acc) ->
  Rec = to_record(Expr),
  {ok, Children} = child_records(Expr, Rec#expr.children),
  Rec2 = Rec#expr{children = Children},
  to_records(Exprs, [Rec2|Acc]).

child_records(#{type := Type}, undefined) when Type =:= list orelse Type =:= tuple orelse Type =:= call ->
  {ok, []};
child_records(#{type := Type}, Children) when Type =:= list orelse Type =:= tuple orelse Type =:= call ->
  Extracted = maps:to_list(Children),
  Sorted = lists:sort(fun({A, _}, {B, _}) ->
    A =< B
  end, Extracted),
  Values = [Val || {_Key, Val} <- Sorted],
  to_records(Values, []);
child_records(#{type := map}, Children) ->
  KVs = [begin
    {ok, [KV]} = to_records([#{type => tuple, line => get_value(line, Val), children => #{
      0 => #{type => literal, value => Key},
      1 => Val
    }}], []),
    KV
  end || {Key, Val} <- maps:to_list(Children)],
  {ok, KVs};
child_records(#{type := 'cond'}, #{0 := Main, 1 := Truthy, 2 := Falsy}) ->
  to_records([Main, Truthy, Falsy], []);
child_records(#{type := 'cond'}, #{0 := Main, 1 := Truthy}) ->
  to_records([Main, Truthy, #{type => literal, value => undefined}], []);
child_records(#{type := comprehension}, #{assignment := #{type := assign, value := Var, children := #{0 := List}}, expression := Expression}) ->
  {ok, [ListRec, ExprRec]} = to_records([
    List,
    Expression
  ], []),
  {ok, [ListRec, Var, ExprRec]};
child_records(#{type := 'assign'}, #{0 := Expr}) ->
  to_records([Expr], []);
child_records(#{type := variable}, undefined) ->
  {ok, undefined};
child_records(#{type := literal}, undefined) ->
  {ok, undefined}.

to_record(Expr) ->
  #expr{
    type = get_value(type, Expr),
    value = get_value(value, Expr),
    line = get_value(line, Expr),
    attrs = get_value(attrs, Expr),
    spawn = get_value(spawn, Expr),
    timeout = get_value(timeout, Expr),
    silent = get_value(silent, Expr),
    children = get_value(children, Expr)
  }.

get_value(Key, Map) ->
  case maps:find(Key, Map) of
    {ok, Value} ->
      Value;
    _ ->
      undefined
  end.

mark_root(Exprs, _) ->
  [Expr|Rest] = lists:reverse(Exprs),
  {ok, lists:reverse([Expr#expr{is_root = true}|Rest])}.

init_state(Exprs, _) ->
  {ok, #state{pending = Exprs}}.

