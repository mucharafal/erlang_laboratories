%%%-------------------------------------------------------------------
%%% @author rafal
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. maj 2018 11:31
%%%-------------------------------------------------------------------
-module(pollution_server_sup).
-author("rafal").


%% API
-export([start/0, stop/0, terminate/0, loop/0]).

start() ->
  register(monitorPollutionServer, spawn(pollution_server_sup, loop, [])).

loop() ->
  process_flag(trap_exit, true),
  pollution_server:start_link(),
  receive
    {'EXIT', Pid, Reason} ->
      io:format("Supervisor: ~w ~w~n", [Reason, Pid]),
      loop();
    {stop} ->
      terminate()
  end.

terminate() ->
  ok.

stop() ->
  terminate().

