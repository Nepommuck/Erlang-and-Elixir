-module(pollution_server).
-author("pawel").

-export([
  start/0, stop/0,
  add_station/2, add_value/4, remove_value/3,
  get_one_value/3, get_station_mean/2, get_daily_mean/2, get_correlation/3]).


%% EXPORTED:
start() ->
  register(server, spawn(fun() -> pollution_loop(none) end)),
  server ! init.

stop() ->
  server ! kill.

add_station(Name, Coordinates) ->
  server ! {add_station, {Name, Coordinates}}.

add_value(Station_id, Date_time, Reading_type, Reading_value) ->
  server ! {add_value, {Station_id, Date_time, Reading_type, Reading_value}}.

remove_value(Station_id, Reading_date_time, Reading_type) ->
  server ! {remove_value, {Station_id, Reading_date_time, Reading_type}}.

get_one_value(Station_id, Reading_date_time, Reading_type) ->
  server ! {get_one_value, {Station_id, Reading_date_time, Reading_type}, self()},
  receive Result -> Result end.

get_station_mean(Station_id, Reading_type) ->
  server ! {get_station_mean, {Station_id, Reading_type}, self()},
  receive Result -> Result end.

get_daily_mean(Reading_type, Date) ->
  server ! {get_daily_mean, {Reading_type, Date}, self()},
  receive Result -> Result end.

get_correlation(Station_id, Reading_type1, Reading_type2) ->
  server ! {get_correlation, {Station_id, Reading_type1, Reading_type2}, self()},
  receive Result -> Result end.


%% UNEXPORTED:
show_error(Message) ->
  io:format("ERROR: ~w. ~n", [Message]).


pollution_loop(Monitor) ->
  receive
    init ->
      case Monitor of
        none ->
          New_monitor = pollution:create_monitor(),
          pollution_loop(New_monitor);
        _ ->
          show_error("Monitor is already initialised"),
          pollution_loop(Monitor)
      end;

    kill -> ok;

    {add_station, {Name, Coordinates}} ->
      New_monitor = pollution:add_station(Name, Coordinates, Monitor),
      case New_monitor of
        {error, Message} ->
          show_error(Message),
          pollution_loop(Monitor);
        _ ->
          pollution_loop(New_monitor)
      end;

    {add_value, {Station_id, Date_time, Reading_type, Reading_value}} ->
      New_monitor = pollution:add_value(Station_id, Date_time, Reading_type, Reading_value, Monitor),
      case New_monitor of
        {error, Message} ->
          show_error(Message),
          pollution_loop(Monitor);
        _ ->
          pollution_loop(New_monitor)
      end;

    {remove_value, {Station_id, Reading_date_time, Reading_type}} ->
      New_monitor = pollution:remove_value(Station_id, Reading_date_time, Reading_type, Monitor),
      case New_monitor of
        {error, Message} ->
          show_error(Message),
          pollution_loop(Monitor);
        _ ->
          pollution_loop(New_monitor)
      end;

    {get_one_value, {Station_id, Reading_date_time, Reading_type}, PID} ->
      Result = pollution:get_one_value(Station_id, Reading_date_time, Reading_type, Monitor),
      case Result of
        {error, Message} -> show_error(Message);
        _ -> ok
      end,
      PID ! Result,
      pollution_loop(Monitor);

    {get_daily_mean, {Reading_type, Date}, PID} ->
      Result = pollution:get_daily_mean(Reading_type, Date, Monitor),
      case Result of
        {error, Message} -> show_error(Message);
        _ -> ok
      end,
      PID ! Result,
      pollution_loop(Monitor);

    {get_station_mean, {Station_id, Reading_type}, PID} ->
      Result = pollution:get_station_mean(Station_id, Reading_type, Monitor),
      case Result of
        {error, Message} -> show_error(Message);
        _ -> ok
      end,
      PID ! Result,
      pollution_loop(Monitor);

    {get_correlation, {Station_id, Reading_type1, Reading_type2}, PID} ->
      Result = pollution:get_correlation(Station_id, Reading_type1, Reading_type2, Monitor),
      case Result of
        {error, Message} -> show_error(Message);
        _ -> ok
      end,
      PID ! Result,
      pollution_loop(Monitor);

    Unknown_command ->
      io:format("ERROR: Unknown server command: ~w. ~n", [Unknown_command]),
      pollution_loop(Monitor)
  end.
