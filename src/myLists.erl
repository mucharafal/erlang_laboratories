%%%-------------------------------------------------------------------
%%% @author rafal
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. mar 2018 12:01
%%%-------------------------------------------------------------------
-module(myLists).
-author("rafal").

%% API
-export([contains/2, duplicateElements/1, sumFloats/1]).

contains([], _) -> false;
contains([Head|_], Head) -> true;
contains([ _|Tail], Value) -> contains(Tail, Value).

duplicateElements([]) -> [];
duplicateElements([Head| Tail]) -> [Head, Head | duplicateElements(Tail)].

sumFloats([]) -> 0;
sumFloats([Head|Tail]) when is_float(Head) -> Head + sumFloats(Tail);
sumFloats([_|_]) ->
  io:format("Error in sumFloats/1!"),
  {error, is_not_float}.

onp(Expression) ->
  processOnp(string:tokens(Expression, " "), []).

processOnp([], [First]) -> First;
processOnp(["+"|Tail], [First, Second | NumTail]) -> processOnp(Tail, [First + Second| NumTail]);
processOnp(["+"|Tail], [First, Second | NumTail]) -> processOnp(Tail, [First + Second| NumTail]);
processOnp(["-"|Tail], [First, Second | NumTail]) -> processOnp(Tail, [First - Second| NumTail]);
processOnp(["*"|Tail], [First, Second | NumTail]) -> processOnp(Tail, [First * Second| NumTail]);
processOnp(["/"|Tail], [First, Second | NumTail]) -> processOnp(Tail, [First / Second| NumTail]);
processOnp([Num|Tail], Nums) -> processOnp(Tail, [list_to_integer(Num) | Nums]).

