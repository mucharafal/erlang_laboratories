%%%-------------------------------------------------------------------
%%% @author rafal
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. kwi 2018 13:14
%%%-------------------------------------------------------------------
-module(foo).
-author("rafal").

%% API
-export([getDailyMean/3, getDailyMeanNormal1/0, getDailyAverageDataCount/1]).

-record(station, {location, measurements}).
-record(measurement, {type, value, date}).

getDailyMean(Monitor, Date, Type) ->
  List = [ Measurements || {_, Station} <- maps:to_list(Monitor),
    Measurements <- [Station#station.location]],
  List.

getDailyMeanNormal1() ->
  M1 = #{"Krakow" => {station,{100,10},[]}, "Poznan" => {station, {150, 30}, []}},
  M2 = pollution:addValue(M1, "Krakow", {{2016, 2, 11}, {14, 28, 16}}, "PM10", 150),
  Val = getDailyMean(M2, {{2016, 2, 11}, {14, 28, 10}}, "PM10"),
  Val.

getDailyAverageDataCount(Monitor) ->
  Dates = [ {StationName, Measurement#measurement.date} || {StationName, Station} <- maps:to_list(Monitor),
    Measurements <- [Station#station.measurements], Measurement <- Measurements],
  CleanedDates = lists:map(fun ({StationName, {Day, _}})-> {StationName, Day} end, Dates),
  Map = lists:foldl(fun (Key, Map) -> IsKey = maps:is_key(Key, Map),
    if
      IsKey -> maps:update(Key, maps:get(Key, Map) + 1, Map);
      true -> maps:put(Key, 1, Map)
    end
                    end,
    #{}, CleanedDates),

  Map.