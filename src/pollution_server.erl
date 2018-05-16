%%%-------------------------------------------------------------------
%%% @author rafal
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. kwi 2018 19:02
%%%-------------------------------------------------------------------
-module(pollution_server).
-author("rafal").

%% API
-export([start/0, stop/0, addValue/4, addStation/2]).
-export([removeValue/3, getDailyAverageDataCount/0]).
-export([getDailyMean/2, getStationMean/2, getValue/3]).
-export([init/0, crash/0, start_link/0]).

start() ->
  register(pollutionServer, spawn(pollution_server, init, [])).

start_link () ->
  register(pollutionServer, spawn_link(pollution_server, init, [])).

init() ->
  loop(pollution:createMonitor()).

loop(Monitor) ->
  receive
    {request, Pid, addStation, Name, Location} ->
      NewMonitor = pollution:addStation(Monitor, Name, Location),
      Pid ! NewMonitor /= Monitor,
      loop(NewMonitor);

    {request, Pid, addValue, Name, Date, Type, Value} ->
      NewMonitor = pollution:addValue(Monitor, Name, Date, Type, Value),
      Pid ! NewMonitor /= Monitor,
      loop(NewMonitor);

    {request, Pid, removeValue, Name, Date, Type} ->
      NewMonitor = pollution:removeValue(Monitor, Name, Date, Type),
      Pid ! NewMonitor /= Monitor,
      loop(NewMonitor);

    {request, Pid, getValue, Name, Date, Type} ->
      try pollution:getValue(Monitor, Name, Date, Type) of
        Value -> Pid ! Value
      catch
        error:X -> Pid ! -1
      end,
      loop(Monitor);

    {request, Pid, getStationMean, Name, Type} ->
      Pid ! pollution:getStationMean(Monitor, Name, Type),
      loop(Monitor);

    {request, Pid, getDailyMean, Date, Type} ->
      Pid ! pollution:getDailyMean(Monitor, Date, Type),
      loop(Monitor);

    {request, Pid, getDailyAverageDataCount} ->
      Pid ! pollution:getDailyAverageDataCount(Monitor),
      loop(Monitor);

    {stop} ->
      finish();

    {alamakota} ->
      0 / 0
  end.

finish() ->
  ok.

stop() ->
  pollutionServer ! {stop}.

addStation(Name, Location) ->
  pollutionServer ! {request, self(), addStation, Name, Location},
  receive
    true -> io:format("Added~n");
    false -> io:format("Cannot add~n")
  end.

addValue(Name, Date, Type, Value) ->
  pollutionServer ! {request, self(), addValue, Name, Date, Type, Value},
  receive
    true -> io:format("Added~n");
    false -> io:format("Cannot add~n")
  end.

removeValue(Name, Date, Type) ->
  pollutionServer ! {request, self(), removeValue, Name, Date, Type},
  receive
    true -> io:format("Removed~n");
    false -> io:format("Cannot remove~n")
  end.

getValue(Name, Date, Type) ->
  pollutionServer ! {request, self(), getValue, Name, Date, Type},
  receive
    -1 -> io:format("Error, no such value~n"), -1;
    Value ->  Value
  end.

getStationMean(Name, Type) ->
  pollutionServer ! {request, self(), getStationMean, Name, Type},
  receive
    -1 -> io:format("Error, no such value~n"), -1;
    Value ->  Value
  end.

getDailyMean(Date, Type) ->
  pollutionServer ! {request, self(), getDailyMean, Date, Type},
  receive
    -1 -> io:format("Error, no such value~n"), -1;
    Value ->  Value
  end.

getDailyAverageDataCount() ->
  pollutionServer ! {request, self(), getDailyAverageDataCount},
  receive
    -1 -> io:format("Error, no such value~n"), -1;
    Value ->  Value
  end.

crash() ->
  pollutionServer ! {alamakota}.
