-module(expr).

-export([apply/3]).
-export([compile/1]).
-export([execute/3]).

-define(VERSION, v2).

apply(Exprs, MapFun, Context) ->
  {ok, Forms} = expr_compiler:compile(Exprs),
  expr_runtime:execute(Forms, MapFun, Context).

compile(Exprs) ->
  case expr_compiler:compile(Exprs) of
    {ok, Forms} ->
      {ok, {?VERSION, Forms}};
    Error ->
      Error
  end.

execute({?VERSION, Forms}, MapFun, Context) ->
  expr_runtime:exectue(Forms, MapFun, Context);
execute(Forms, _, _) ->
  {error, {unsupported_format, Forms}}.
