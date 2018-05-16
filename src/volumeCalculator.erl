%%%-------------------------------------------------------------------
%%% @author rafal
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. mar 2018 21:01
%%%-------------------------------------------------------------------
-module(volumeCalculator).
-author("rafal").

%% API
-export([volume/2, volume/3, factorial/1, dziwny_case/1, power/2]).

volume(qube, Edge)  ->
  Edge * Edge * Edge;
volume(cuboid, {Edge1, Edge2, Edge3}) ->
  Edge1 * Edge2 * Edge3;
volume(_, _) ->
  io:format("Error in volume/2!"),
  {error, cannot_calculate}.

volume(qubic, Edge1, Edge2) ->
  Edge2 * Edge1 * 1;
volume(_, _, _) ->
  io:format("costam"),
  {error, cannot_calculate}.

factorial(0) -> 1;
factorial(N) -> N * factorial(N-1).

dziwny_case(N) ->
  case N of
    0 -> 1;
    N -> N * dziwny_case(N - 1)
  end.

power(N, 0) -> 1;
power(N, To) -> N * power(N, To - 1).
