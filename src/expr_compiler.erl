-module(expr_compiler).

-export([compile/1]).

-include("expr.hrl").

-define(PASSES, [
  fun to_records/2,
  fun mark_root/2
]).

compile(Exprs) ->
  compile(Exprs, ?PASSES).

compile(Exprs, []) ->
  {ok, Exprs};
compile(Exprs, [Pass|Passes]) ->
  case Pass(Exprs, []) of
    {ok, Exprs2} ->
      compile(Exprs2, Passes);
    Error ->
      Error
  end.

%% convert the maps to the internal records
to_records([], Acc) ->
  {ok, lists:reverse(Acc)};
to_records(undefined, _) ->
  {ok, undefined};
to_records([Expr|Exprs], Acc) ->
  Rec = to_record(Expr),
  {ok, Children} = to_records(Rec#expr.children, []),
  Rec2 = Rec#expr{children = Children},
  to_records(Exprs, [Rec2|Acc]).

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

