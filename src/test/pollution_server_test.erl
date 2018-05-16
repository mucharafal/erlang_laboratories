%%%-------------------------------------------------------------------
%%% @author rafal
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. kwi 2018 20:20
%%%-------------------------------------------------------------------
-module(pollution_server_test).
-include_lib("eunit/include/eunit.hrl").
-author("rafal").

%% API
-export([test/0]).

test() ->
  start_stop_test(),
  getValue_test(),
  removeValue_test(),
  getIncorrectValue_test().


start_stop_test() ->
  ?assert(pollution_server:start()),
  ?assertError(badarg, pollution_server:start()),
  ?assert(pollution_server:stop() == {stop}),
  timer:sleep(1),
  ?assert(pollution_server:start()),
  ?assert(pollution_server:stop() == {stop}),
  timer:sleep(1).


prepare() ->
  pollution_server:start(),

  pollution_server:addStation("Krakow",{10, 120}),
  pollution_server:addValue({10, 120}, {"2018-01-01", "10:00:00"}, "PM10", 120),
  pollution_server:addValue({10, 120}, {"2018-01-01", "10:00:01"}, "PM10", 130),
  pollution_server:addValue("Krakow", {"2018-01-01", "10:00:02"}, "PM10", 140),

  pollution_server:addStation("Poznan", {10, 130}),
  pollution_server:addValue({10, 130}, {"2018-01-01", "10:00:00"}, "PM11", 20),
  pollution_server:addValue({10, 130}, {"2018-02-01", "10:00:01"}, "PM12", 10),
  pollution_server:addValue("Krakow", {"2018-03-01", "10:00:02"}, "PM12", 40).

getValue_test() ->
  prepare(),

  ?assertEqual(120, pollution_server:getValue("Krakow", {"2018-01-01", "10:00:00"}, "PM10")),
  ?assertEqual(130, pollution_server:getValue("Krakow", {"2018-01-01", "10:00:01"}, "PM10")),
  ?assertEqual(140, pollution_server:getValue({10, 120}, {"2018-01-01", "10:00:02"}, "PM10")),
  ?assertEqual(40, pollution_server:getValue("Krakow", {"2018-03-01", "10:00:02"}, "PM12")),

  ?assertEqual(20, pollution_server:getValue("Poznan", {"2018-01-01", "10:00:00"}, "PM11")),
  ?assertEqual(10, pollution_server:getValue({10, 130}, {"2018-02-01", "10:00:01"}, "PM12")),

  pollution_server:stop().

getIncorrectValue_test() ->
  prepare(),

  %Place
  ?assertEqual(-1, pollution_server:getValue("Krako", {"2018-01-01", "10:00:00"}, "PM10")),
  %Date
  ?assertEqual(-1, pollution_server:getValue("Krakow", {"2018-01-02", "10:00:01"}, "PM10")),
  %Hour
  ?assertEqual(-1, pollution_server:getValue({10, 120}, {"2018-01-01", "10:02:02"}, "PM10")),
  %Type
  ?assertEqual(-1, pollution_server:getValue("Krakow", {"2018-03-01", "10:00:02"}, "PM11")),

  %Measurement exist in other place
  ?assertEqual(-1, pollution_server:getValue("Poznan", {"2018-01-01", "10:00:00"}, "PM10")),
  %Coordinates
  ?assertEqual(-1, pollution_server:getValue({10, 10}, {"2018-02-01", "10:00:01"}, "PM12")),

  pollution_server:stop().

removeValue_test() ->
  prepare(),

  %remove by name
  ?assertEqual(120, pollution_server:getValue("Krakow", {"2018-01-01", "10:00:00"}, "PM10")),
  pollution_server:removeValue("Krakow", {"2018-01-01", "10:00:00"}, "PM10"),
  ?assertEqual(-1, pollution_server:getValue("Krakow", {"2018-01-01", "10:00:00"}, "PM10")),
  ?assertEqual(130, pollution_server:getValue("Krakow", {"2018-01-01", "10:00:01"}, "PM10")),
  ?assertEqual(140, pollution_server:getValue({10, 120}, {"2018-01-01", "10:00:02"}, "PM10")),
  ?assertEqual(40, pollution_server:getValue("Krakow", {"2018-03-01", "10:00:02"}, "PM12")),

  ?assertEqual(20, pollution_server:getValue("Poznan", {"2018-01-01", "10:00:00"}, "PM11")),
  ?assertEqual(10, pollution_server:getValue({10, 130}, {"2018-02-01", "10:00:01"}, "PM12")),

  pollution_server:stop(),

  timer:sleep(1),

  prepare(),
  %also by name, another station
  ?assertEqual(120, pollution_server:getValue("Krakow", {"2018-01-01", "10:00:00"}, "PM10")),
  pollution_server:removeValue({10, 130}, {"2018-02-01", "10:00:01"}, "PM12"),
  ?assertEqual(120, pollution_server:getValue("Krakow", {"2018-01-01", "10:00:00"}, "PM10")),
  ?assertEqual(130, pollution_server:getValue("Krakow", {"2018-01-01", "10:00:01"}, "PM10")),
  ?assertEqual(140, pollution_server:getValue({10, 120}, {"2018-01-01", "10:00:02"}, "PM10")),
  ?assertEqual(40, pollution_server:getValue("Krakow", {"2018-03-01", "10:00:02"}, "PM12")),

  ?assertEqual(20, pollution_server:getValue("Poznan", {"2018-01-01", "10:00:00"}, "PM11")),
  ?assertEqual(-1, pollution_server:getValue({10, 130}, {"2018-02-01", "10:00:01"}, "PM12")),

  pollution_server:stop(),

  timer:sleep(1),

  prepare(),
  %remove by coordinates
  ?assertEqual(120, pollution_server:getValue("Krakow", {"2018-01-01", "10:00:00"}, "PM10")),
  pollution_server:removeValue({10, 130}, {"2018-01-01", "10:00:00"}, "PM11"),
  ?assertEqual(120, pollution_server:getValue("Krakow", {"2018-01-01", "10:00:00"}, "PM10")),
  ?assertEqual(130, pollution_server:getValue("Krakow", {"2018-01-01", "10:00:01"}, "PM10")),
  ?assertEqual(140, pollution_server:getValue({10, 120}, {"2018-01-01", "10:00:02"}, "PM10")),
  ?assertEqual(40, pollution_server:getValue("Krakow", {"2018-03-01", "10:00:02"}, "PM12")),

  ?assertEqual(-1, pollution_server:getValue("Poznan", {"2018-01-01", "10:00:00"}, "PM11")),
  ?assertEqual(10, pollution_server:getValue({10, 130}, {"2018-02-01", "10:00:01"}, "PM12")),

  pollution_server:stop(),

  timer:sleep(1),

  prepare(),
  %remove from no existing station
  ?assertEqual(120, pollution_server:getValue("Krakow", {"2018-01-01", "10:00:00"}, "PM10")),
  pollution_server:removeValue({10, 1}, {"2018-02-01", "10:00:01"}, "PM12"),
  ?assertEqual(120, pollution_server:getValue("Krakow", {"2018-01-01", "10:00:00"}, "PM10")),
  ?assertEqual(130, pollution_server:getValue("Krakow", {"2018-01-01", "10:00:01"}, "PM10")),
  ?assertEqual(140, pollution_server:getValue({10, 120}, {"2018-01-01", "10:00:02"}, "PM10")),
  ?assertEqual(40, pollution_server:getValue("Krakow", {"2018-03-01", "10:00:02"}, "PM12")),

  ?assertEqual(20, pollution_server:getValue("Poznan", {"2018-01-01", "10:00:00"}, "PM11")),
  ?assertEqual(10, pollution_server:getValue({10, 130}, {"2018-02-01", "10:00:01"}, "PM12")),

  pollution_server:stop(),

  timer:sleep(1),

  prepare(),
  %incorrect date
  ?assertEqual(120, pollution_server:getValue("Krakow", {"2018-01-01", "10:00:00"}, "PM10")),
  pollution_server:removeValue({10, 130}, {"2018-03-01", "10:00:01"}, "PM12"),
  ?assertEqual(120, pollution_server:getValue("Krakow", {"2018-01-01", "10:00:00"}, "PM10")),
  ?assertEqual(130, pollution_server:getValue("Krakow", {"2018-01-01", "10:00:01"}, "PM10")),
  ?assertEqual(140, pollution_server:getValue({10, 120}, {"2018-01-01", "10:00:02"}, "PM10")),
  ?assertEqual(40, pollution_server:getValue("Krakow", {"2018-03-01", "10:00:02"}, "PM12")),

  ?assertEqual(20, pollution_server:getValue("Poznan", {"2018-01-01", "10:00:00"}, "PM11")),
  ?assertEqual(10, pollution_server:getValue({10, 130}, {"2018-02-01", "10:00:01"}, "PM12")),

  pollution_server:stop(),

  timer:sleep(1),

  prepare(),
  %incorrect hour
  ?assertEqual(120, pollution_server:getValue("Krakow", {"2018-01-01", "10:00:00"}, "PM10")),
  pollution_server:removeValue({10, 130}, {"2018-02-01", "10:00:00"}, "PM12"),
  ?assertEqual(120, pollution_server:getValue("Krakow", {"2018-01-01", "10:00:00"}, "PM10")),
  ?assertEqual(130, pollution_server:getValue("Krakow", {"2018-01-01", "10:00:01"}, "PM10")),
  ?assertEqual(140, pollution_server:getValue({10, 120}, {"2018-01-01", "10:00:02"}, "PM10")),
  ?assertEqual(40, pollution_server:getValue("Krakow", {"2018-03-01", "10:00:02"}, "PM12")),

  ?assertEqual(20, pollution_server:getValue("Poznan", {"2018-01-01", "10:00:00"}, "PM11")),
  ?assertEqual(10, pollution_server:getValue({10, 130}, {"2018-02-01", "10:00:01"}, "PM12")),

  pollution_server:stop(),

  timer:sleep(1),

  prepare(),
  %incorrect type
  ?assertEqual(120, pollution_server:getValue("Krakow", {"2018-01-01", "10:00:00"}, "PM10")),
  pollution_server:removeValue({10, 130}, {"2018-02-01", "10:00:01"}, "PM11"),
  ?assertEqual(120, pollution_server:getValue("Krakow", {"2018-01-01", "10:00:00"}, "PM10")),
  ?assertEqual(130, pollution_server:getValue("Krakow", {"2018-01-01", "10:00:01"}, "PM10")),
  ?assertEqual(140, pollution_server:getValue({10, 120}, {"2018-01-01", "10:00:02"}, "PM10")),
  ?assertEqual(40, pollution_server:getValue("Krakow", {"2018-03-01", "10:00:02"}, "PM12")),

  ?assertEqual(20, pollution_server:getValue("Poznan", {"2018-01-01", "10:00:00"}, "PM11")),
  ?assertEqual(10, pollution_server:getValue({10, 130}, {"2018-02-01", "10:00:01"}, "PM12")),

  pollution_server:stop().

getStationMean_test() ->
  prepare(),

  %correct values
  ?assertEqual(130.0, pollution_server:getStationMean("Krakow", "PM10")),
  ?assertEqual(20.0, pollution_server:getStationMean({10, 130}, "PM11")),
  ?assertEqual(40.0, pollution_server:getStationMean("Krakow", "PM12")),

  %incorrect type
  ?assertEqual(-1, pollution_server:getStationMean("Krakow", "PM13")),

  %incorrect station name
  ?assertEqual(-1, pollution_server:getStationMean("Krakw", "PM10")),

  %incorrect station name
  ?assertEqual(-1, pollution_server:getStationMean({1, 10}, "PM10")),
  pollution_server:stop().

getDailyMean_test() ->
  prepare(),

  pollution_server:addValue("Poznan", {"2018-01-01", "10:09:10"}, "PM10", 0),

  %correct values
  ?assertEqual(97.5, pollution_server:getDailyMean({"2018-01-01", "sth"}, "PM10")),
  ?assertEqual(40.0, pollution_server:getDailyMean({"2018-03-01", "sth"}, "PM12")),

  %incorrect date
  ?assertEqual(-1, pollution_server:getDailyMean({"2017-01-01", "sth"}, "PM10")),

  %incorrect type
  ?assertEqual(-1, pollution_server:getDailyMean({"2018-01-01", "sth"}, "PM13")),

  pollution_server:stop().

getDailyAvgDC_test() ->
  prepare(),

  ?assertEqual(6/4, pollution_server:getDailyAverageDataCount()),

  pollution_server:addValue("Poznan", {"2018-01-01", "10:09:10"}, "PM18", 0),

  %correct values
  ?assertEqual(7/4, pollution_server:getDailyAverageDataCount()),

  pollution_server:stop().

