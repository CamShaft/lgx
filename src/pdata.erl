-module(pdata).

-export([compile/2]).
-export([execute/3]).
-export([apply/4]).

compile(Graph, _Start) ->
  {ok, Graph}.

execute(_Forms, _MapFun, _Context) ->
  {ok, 1}.

apply(Graph, Start, MapFun, Context) ->
  {ok, Forms} = compile(Graph, Start),
  execute(Forms, MapFun, Context).
