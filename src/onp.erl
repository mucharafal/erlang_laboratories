%%%-------------------------------------------------------------------
%%% @author rafal
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. kwi 2018 23:33
%%%-------------------------------------------------------------------
-module(onp).
-author("rafal").

%% API
-export([onp/1]).

onp(Expression) ->
  X = string:tokens(Expression, " "),
  evaluateOnp(X, []).

evaluateOnp([], [A]) -> A;
evaluateOnp(["+" | Tail], [F , S | Tail2]) -> evaluateOnp(Tail, [F + S | Tail2]);
evaluateOnp(["-" | Tail], [F , S | Tail2]) -> evaluateOnp(Tail, [S - F | Tail2]);
evaluateOnp(["*" | Tail], [F , S | Tail2]) -> evaluateOnp(Tail, [F * S | Tail2]);
evaluateOnp(["/" | Tail], [F , S | Tail2]) -> evaluateOnp(Tail, [S / F | Tail2]);
evaluateOnp(["pow" | Tail], [F , S | Tail2]) -> evaluateOnp(Tail, [math:pow(S, F) | Tail2]);
evaluateOnp(["sqrt" | Tail], [F | Tail2]) -> evaluateOnp(Tail, [math:sqrt(F) | Tail2]);
evaluateOnp(["sin" | Tail], [F | Tail2]) -> evaluateOnp(Tail, [math:sin(F) | Tail2]);
evaluateOnp([Head | Tail], Args) ->
  try list_to_integer(Head) of
    X -> evaluateOnp( Tail, [X | Args])
  catch
    error:badarg  -> evaluateOnp( Tail, [list_to_float(Head) | Args])
  end.
