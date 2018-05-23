%%%-------------------------------------------------------------------
%%% @author rafal
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. maj 2018 10:03
%%%-------------------------------------------------------------------
-module(pollution_var_server).
-author("rafal").
-behaviour(gen_server).

%% API
-export([start_link/1, init/1, handle_call/3]).

-export([handle_cast/2, getMonitor/0, setMonitor/1]).


start_link(InitialValue) ->
  io:format("Starting var server~n"),
  gen_server:start_link({local, pollution_var_server},
    pollution_var_server,
    InitialValue, []).

init(InitialValue) ->
  {ok, InitialValue}.

getMonitor() ->
  io:format("GetVal~n"),
  gen_server:call(pollution_var_server, {getMonitor}).

setMonitor(Monitor) ->
  io:format("SetVal~n"),
  gen_server:cast(pollution_var_server, {setMonitor, Monitor}).

handle_call({getMonitor}, _, Monitor) ->
  io:format("~w", [Monitor]),
  {reply, Monitor, Monitor}.

handle_cast({setMonitor, Monitor}, _) ->
  io:format("~w", [Monitor]),
  {noreply, Monitor}.