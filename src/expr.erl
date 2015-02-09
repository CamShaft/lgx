-module(expr).

-export([apply/3]).
-export([compile/1]).
-export([execute/3]).
-export([context/1]).

-include("expr.hrl").

-define(VERSION, v2).

apply(Exprs, MapFun, Context) ->
  {ok, Forms} = expr_compiler:compile(Exprs),
  expr_runtime:execute(Forms, MapFun, Context).

compile(Exprs) ->
  case expr_compiler:compile(Exprs) of
    {ok, Forms} ->
      %% get the ast ready for pending executions
      case expr_runtime_pending:loop(Forms) of
        {ok, PendingState} ->
          {ok, {?VERSION, PendingState}};
        {error, Error, State} ->
          {error, Error, State};
        {ok, Value, State} ->
          {ok, {?VERSION, Value, State}}
      end;
    Error ->
      Error
  end.

execute({?VERSION, Forms}, MapFun, Context) ->
  expr_runtime:execute(Forms, MapFun, Context);
execute({?VERSION, Value, State}, _, _) ->
  {ok, Value, State};
execute(Forms, _, _) ->
  {error, {unsupported_format, Forms}}.

context(#state{context = Context}) ->
  Context.

-ifdef(PERF).
horse_execute() ->
    horse:repeat(2000, benchmarks_test:execute()).
-endif.
