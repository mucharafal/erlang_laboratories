%%%-------------------------------------------------------------------
%%% @author rafal
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. kwi 2018 12:31
%%%-------------------------------------------------------------------
-module(pollution).
-author("rafal").

%% API
-export([createMonitor/0, addStation/3, addValue/5,
  removeValue/4, getValue/4, getStationMean/3,
  getDailyMean/3, getDailyAverageDataCount/1]).
-record(station, {location, measurements}).
-record(measurement, {type, value, date}).

createMonitor() ->
  #{}.

addStation(Monitor, Name, Location) ->
  IsUnique = not maps:is_key(Name, Monitor) and (not isLocation(Location, Monitor)),
  if
    IsUnique -> maps:put(Name, #station{location = Location, measurements = []}, Monitor);
    true  -> Monitor
  end.

isLocation(Location, Monitor) ->
  ListForm = maps:to_list(Monitor),
  lists:any(fun ({_, Station}) -> Station#station.location == Location end, ListForm).

addValue(Monitor, Name, Date, Type, Value) when is_list(Name) ->
  IsStation = maps:is_key(Name, Monitor),
  if
    IsStation   -> Monitor#{Name => addMeasurement(maps:get(Name, Monitor), Date, Type, Value)};
    true        -> Monitor
  end;
addValue(Monitor, Location, Date, Type, Value) ->
  IsLocation = isLocation(Location, Monitor),
  if
    IsLocation ->
      Name = getStationNameByLocation(Location, Monitor),
      addValue(Monitor, Name, Date, Type, Value);
    true  -> Monitor
  end.

getStationNameByLocation(Location, Monitor) ->
  ListForm = maps:to_list(Monitor),
  [Name] = [X || {X, Record} <- ListForm, Record#station.location == Location],
  Name.

addMeasurement(Station, Date, Type, Value) ->
  Location = Station#station.location,
  List = Station#station.measurements,
  IsSuchMeasurement = lists:any(fun (X) -> ((Date == X#measurement.date) and (Type == X#measurement.type)) end, List),
  if
    IsSuchMeasurement -> Station;
    true -> #station{location = Location, measurements = [#measurement{date = Date, type = Type, value = Value} | List]}
  end.

removeValue(Monitor, Name, Date, Type) when is_list(Name) ->
  IsStation = maps:is_key(Name, Monitor),
  if
    IsStation ->
      Location = (maps:get(Name, Monitor))#station.location,
      MeasuresList = (maps:get(Name, Monitor))#station.measurements,
      NewList = [X || X <- MeasuresList, not ((X#measurement.type == Type) and (X#measurement.date == Date))],
      maps:put(Name, #station{location = Location, measurements = NewList}, Monitor);
    true -> Monitor
  end;
removeValue(Monitor, Location, Date, Type) ->
  IsLocation = isLocation(Location, Monitor),
  if
    IsLocation ->
      Name = getStationNameByLocation(Location, Monitor),
      removeValue(Monitor, Name, Date, Type);
    true  -> Monitor
  end.

getValue(Monitor, Name, Date, Type) when is_list(Name) ->
  IsStation = maps:is_key(Name, Monitor),
  if
    IsStation ->
      Measurements = (maps:get(Name, Monitor))#station.measurements,
      [Value] = [X#measurement.value || X <- Measurements, X#measurement.date == Date, X#measurement.type == Type],
      Value;
    true  -> -1
  end;
getValue(Monitor, Location, Date, Type) ->
  IsLocation = isLocation(Location, Monitor),
  if
    IsLocation ->
      Name = getStationNameByLocation(Location, Monitor),
      getValue(Monitor, Name, Date, Type);
    true  -> -1
  end.

getStationMean(Monitor, Name, Type) when is_list(Name)->
  IsStation = maps:is_key(Name, Monitor),
  if
    IsStation ->
      Measurements = (maps:get(Name, Monitor))#station.measurements,
      List = [X#measurement.value || X <- Measurements, X#measurement.type == Type],
      ListLen = lists:foldl(fun (_, Y) -> Y + 1 end, 0, List),
      if
        ListLen == 0 -> -1;
        true -> (lists:foldl(fun (X, Y) -> X + Y end, 0, List) / ListLen)
      end;
    true -> -1
  end;
getStationMean(Monitor, Location, Type) ->
  IsLocation = isLocation(Location, Monitor),
  if
    IsLocation ->
      Name = getStationNameByLocation(Location, Monitor),
      getStationMean(Monitor, Name, Type);
    true  -> -1
  end.

getDailyMean(Monitor, Date, Type) ->
  List = [ Measurement#measurement.value || {_, Station} <- maps:to_list(Monitor),
    Measurements <- [Station#station.measurements],
    Measurement <- Measurements,
    theSameDay(Measurement#measurement.date, Date), Measurement#measurement.type == Type],
  ListLen = lists:foldl(fun (_, Y) -> Y + 1 end, 0, List),
  if
    ListLen > 0 -> lists:foldl(fun (X, Y) -> X + Y end, 0, List) / ListLen;
    true -> -1
  end.

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

  List = maps:to_list(Map),

  ListLen = lists:foldl(fun (_, Y) -> Y + 1 end, 0, List),
  if
    ListLen > 0 -> lists:foldl(fun ({_, Value}, Y) -> Value + Y end, 0, List) / ListLen;
    true -> -1
  end.

theSameDay(Date1, Date2) ->
  {Day1, _ } = Date1,
  {Day2, _ } = Date2,
  Day1 == Day2.
