%%%-------------------------------------------------------------------
%%% @author rafal
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. kwi 2018 11:38
%%%-------------------------------------------------------------------
-module(qsort).
-author("rafal").

%% API
-export([qs/1, randomElems/3, compareSpeeds/3, sumDigits2/1]).

lessThan(List, Arg) -> lists:filter(fun (X) -> X < Arg end, List).
grtEqThan(List, Arg) -> lists:filter(fun (X) -> X >= Arg end, List).
qs([]) -> [];
qs([Pivot|Tail]) -> qs(lessThan(Tail, Pivot)) ++ [Pivot] ++ qs( grtEqThan(Tail, Pivot)).

randomElems(N, Min, Max) -> lists:map(fun (X) -> rand:uniform() * (Max - Min) + Min end, lists:seq(1, N)).

compareSpeeds(List, Fun1, Fun2) ->
  {{Fun1Res, _}, {Fun2Res, _}} = {timer:tc(Fun1, [List]), timer:tc(Fun2, [List])},
    {Fun1Res, Fun2Res}.

sumDigits2 (Number) -> lists:foldl(fun (X, Y) -> X + Y end, 0, [X - 48 || X <- integer_to_list(Number)]).
