-module(expr_runtime).

-include("expr.hrl").

-export([execute/3]).

execute([], _, _) ->
  {ok, undefined};
%% they made it easy for us :D
execute([#expr{type = literal, value = Value}], _, _) ->
  {ok, Value};
execute(Forms, _MapFun, _Context) ->
  {todo, Forms}.
