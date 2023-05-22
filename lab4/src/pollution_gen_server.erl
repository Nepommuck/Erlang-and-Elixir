-module(pollution_gen_server).
-author("pawel").

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

%% "Procedures"
-export([add_station/2, add_value/4, remove_value/3]).
%% "Functions"
-export([get/0, get_one_value/3, get_daily_mean/2, get_correlation/3]).

-define(SERVER_NAME, ?MODULE).
-define(INITIAL_VALUE, none).


start_link() ->
  gen_server:start_link({local, ?SERVER_NAME}, ?MODULE, ?INITIAL_VALUE, []).

init(?INITIAL_VALUE) ->
  {ok, pollution:create_monitor()}.


%% User - Server Interface
%%======================================================================================================================
add_station(Name, Coordinates) ->
  gen_server:cast(?MODULE, {add_station, Name, Coordinates}).

add_value(Station_id, Date_time, Reading_type, Reading_value) ->
  gen_server:cast(?MODULE, {add_value, Station_id, Date_time, Reading_type, Reading_value}).

remove_value(Station_id, Reading_date_time, Reading_type) ->
  gen_server:cast(?MODULE, {remove_value, Station_id, Reading_date_time, Reading_type}).


get() ->
  gen_server:call(?MODULE, get).

get_one_value(Station_id, Reading_date_time, Reading_type) ->
  gen_server:call(?MODULE, {get_one_value, Station_id, Reading_date_time, Reading_type}).

get_daily_mean(Reading_type, Date) ->
  gen_server:call(?MODULE, {get_daily_mean, Reading_type, Date}).

get_correlation(Station_id, Reading_type1, Reading_type2) ->
  gen_server:call(?MODULE, {get_correlation, Station_id, Reading_type1, Reading_type2}).


handle_cast(Cast_arguments, Current_monitor) ->
  Result = case Cast_arguments of
             {add_station, Name, Coordinates} ->
               pollution:add_station(Name, Coordinates, Current_monitor);

             {add_value, Station_id, Date_time, Reading_type, Reading_value} ->
               pollution:add_value(Station_id, Date_time, Reading_type, Reading_value, Current_monitor);

             {remove_value, Station_id, Reading_date_time, Reading_type} ->
               pollution:remove_value(Station_id, Reading_date_time, Reading_type, Current_monitor);

             _ ->
               {error, unknown_cast_argument}
           end,

  case Result of
    {error, Error_message} ->
%%      {stop, Error_message, Current_monitor};
      print_error(Error_message),
      {noreply, Current_monitor};
    _ ->
      {noreply, Result}
  end.


handle_call(Call_arguments, _From, Current_monitor) ->
  Result = case Call_arguments of
             get ->
               Current_monitor;

             {get_one_value, Station_id, Reading_date_time, Reading_type} ->
               pollution:get_one_value(Station_id, Reading_date_time, Reading_type, Current_monitor);

             {get_station_mean, Station_id, Reading_type} ->
               pollution:get_station_mean(Station_id, Reading_type, Current_monitor);

             {get_daily_mean, Reading_type, Date} ->
               pollution:get_daily_mean(Reading_type, Date, Current_monitor);

             {get_correlation, Station_id, Reading_type1, Reading_type2} ->
               pollution:get_correlation(Station_id, Reading_type1, Reading_type2, Current_monitor)
           end,

  case Result of
    {error, Error_message} ->
%%      {stop, Error_message, Current_monitor};
      print_error(Error_message),
      {noreply, Current_monitor};
    _ ->
      {reply, Result, Current_monitor}
  end.


crash() ->
  no:exist(),
  ok.


print_error(Error_message) ->
  io:format("ERROR: ~w. ~n", [Error_message]),
  ok.


terminate(Reason, State) ->
  io:format("ERROR: ~w. ~n", [Reason]),
  io:format("Last state was: ~w. ~n", [State]),
  ok.
