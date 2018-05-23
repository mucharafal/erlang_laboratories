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
-export([getDailyAverageDataCount/0, getDailyMean/2, getStationMean/2]).
-export([getValue/3, handle_cast/2]).

init(InitialValue) ->
  {ok, InitialValue}.

start_link(_) ->
  gen_server:start_link(
    {local, pollutionServer},
    pollution_gen_server, pollution_var_server:getMonitor(), []
  ).

start(_) ->
  gen_server:start({local, pollutionServer},
    pollution_gen_server, pollution_var_server:getMonitor(), [] ).

handle_call({addStation, [Name, Location]}, _From, Monitor) ->
  NewMonitor = pollution:addStation(Monitor, Name, Location),
  pollution_var_server:setMonitor(NewMonitor),
  {reply, Monitor /= NewMonitor, NewMonitor};
handle_call({addValue, [Name, Date, Type, Value]}, _From, Monitor) ->
  NewMonitor = pollution:addValue(Monitor, Name, Date, Type, Value),
  pollution_var_server:setMonitor(NewMonitor),
  {reply, Monitor /= NewMonitor, NewMonitor};
handle_call({removeValue, [Name, Date, Type]}, _From, Monitor) ->
  NewMonitor = pollution:removeValue(Monitor, Name, Date, Type),
  pollution_var_server:setMonitor(NewMonitor),
  {reply, Monitor /= NewMonitor, NewMonitor};
handle_call({getValue, [Name, Date, Type]}, _From, Monitor) ->
  try pollution:getValue(Monitor, Name, Date, Type) of
    Value -> {reply, Value, Monitor}
  catch
    error: _ -> {reply, -1, Monitor}
  end;
handle_call({getStationMean, [Name, Type]}, _From, Monitor) ->
  {reply, pollution:getStationMean(Monitor, Name, Type), Monitor};
handle_call({getDailyMean, [Date, Type]}, _From, Monitor) ->
  {reply, pollution:getDailyMean(Monitor, Date, Type), Monitor};
handle_call({getDailyAverageDataCount}, _From, Monitor) ->
  {reply, pollution:getDailyAverageDataCount(Monitor), Monitor}.

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

getValue(Name, Date, Type) ->
  case gen_server:call(pollutionServer, {getValue, [Name, Date, Type]}) of
    -1 -> io:format("Error, no such value~n"), -1;
    Value ->  Value
  end.

getStationMean(Name, Type) ->
  case gen_server:call(pollutionServer, {getStationMean, [Name, Type]}) of
    -1 -> io:format("Error, no such value~n"), -1;
    Value ->  Value
  end.

getDailyMean(Date, Type) ->
  case gen_server:call(pollutionServer, {getDailyMean, [Date, Type]}) of
    -1 -> io:format("Error, no such value~n"), -1;
    Value ->  Value
  end.

getDailyAverageDataCount() ->
  case gen_server:call(pollutionServer, {getStationMean}) of
    -1 -> io:format("Error, no such value~n"), -1;
    Value ->  Value
  end.

crash() ->
  gen_server:cast(pollutionServer, {crash}).

handle_cast({crash}, _) ->
  0 / 0.








