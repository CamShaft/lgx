-module(expr_compiler).

-export([compile/1]).

-include("expr.hrl").

-define(PASSES, [
  fun to_records/1,
  fun mark_root/1,
  fun init_state/1,
  fun rename_variables/1
]).

compile(Exprs) ->
  compile(Exprs, ?PASSES).

compile(Exprs, []) ->
  {ok, Exprs};
compile(Exprs, [Pass|Passes]) ->
  {ok, Exprs2} = Pass(Exprs),
  compile(Exprs2, Passes).

to_records(Exprs) ->
  to_records(Exprs, []).

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

mark_root(Exprs) ->
  [Expr|Rest] = lists:reverse(Exprs),
  {ok, lists:reverse([Expr#expr{is_root = true}|Rest])}.

init_state(Exprs) ->
  [Root|Rest] = lists:reverse(Exprs),
  State = #state{pending = [Root]},
  State2 = lists:foldl(fun set_vars/2, State, Rest),
  {ok, State2}.

set_vars(#expr{type = assign, value = Var, children = [Expr]}, State) ->
  Vars = State#state.vars,
  State#state{vars = maps:put(Var, Expr, Vars)}.

rename_variables(State) ->
  Vars = State#state.vars,
  {Mappings, State2} = maps:fold(fun rename_variables/3, {#{}, State}, Vars),
  State3 = maps:fold(fun replace_variables/3, State2, Mappings),
  {ok, State3}.

rename_variables(Key, _Val, {Mappings, State}) ->
  {ID, State2} = expr_util:next_id(State),
  {maps:put(Key, ID, Mappings), State2}.

replace_variables(Var, ID, State) ->
  Expr = maps:get(Var, State#state.vars),
  Vars = maps:remove(Var, State#state.vars),
  {ok, Pending} = expr_util:replace_variable(Var, ID, State#state.pending, []),
  Vars2 = maps:fold(fun(Key, Val, Acc) ->
    {ok, [Val2]} = expr_util:replace_variable(Var, ID, [Val], []),
    maps:put(Key, Val2, Acc)
  end, #{}, Vars),
  Vars3 = maps:put(ID, Expr#expr{id = ID}, Vars2),
  State#state{vars = Vars3, pending = Pending}.
