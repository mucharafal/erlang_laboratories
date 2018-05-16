%%%-------------------------------------------------------------------
%%% @author rafal
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. maj 2018 12:07
%%%-------------------------------------------------------------------
-module(pollution_gen_server).
-author("rafal").
-behavior(gen_server).

%% API
-export([init/1, start_link/1, start/1, handle_call/3, addStation/2]).
-export([addValue/4, removeValue/3, crash/0, handle_call/3]).

init(Args) ->
  {ok, pollution:createMonitor()}.

start_link(InitialValue) ->
  gen_server:start_link(
    {local, pollutionServer},
    pollution_gen_server, InitialValue, []
  ).

start(InitialValue) ->
  gen_server:start({local, pollutionServer},
    pollution_gen_server, InitialValue, [] ).

handle_call({addStation, [Name, Location]}, _From, Monitor) ->
  NewMonitor = pollution:addStation(Monitor, Name, Location),
  {reply, Monitor /= NewMonitor, NewMonitor};
handle_call({addValue, [Name, Date, Type, Value]}, _From, Monitor) ->
  NewMonitor = pollution:addValue(Monitor, Name, Date, Type, Value),
  {reply, Monitor /= NewMonitor, NewMonitor};
handle_call({removeValue, [Name, Date, Type]}, _From, Monitor) ->
  NewMonitor = pollution:removeValue(Monitor, Name, Date, Type),
  {reply, Monitor /= NewMonitor, NewMonitor}.

addStation(Name, Location) ->
  case gen_server:call(pollutionServer, {addStation, [Name, Location]}) of
    true -> io:format("Added~n");
    false -> io:format("Cannot add~n")
  end.

addValue(Name, Date, Type, Value) ->
  case gen_server:call(pollutionServer, {addValue, [Name, Date, Type, Value]}) of
    true -> io:format("Added~n");
    false -> io:format("Cannot add~n")
  end.

removeValue(Name, Date, Type) ->
  case gen_server:call(pollutionServer, {removeValue, [Name, Date, Type]}) of
    true -> io:format("Added~n");
    false -> io:format("Cannot add~n")
  end.

crash() ->
  gen_server:cast(pollutionServer, {crash}).

handle_cast({crash}, Monitor) ->
  0 / 0.








