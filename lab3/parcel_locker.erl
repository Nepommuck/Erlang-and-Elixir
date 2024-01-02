-module(parcel_locker).
-author("pawel").

%% API
-export([get_random_coordinates/2, report/0, report/2,
  find_min_distance_sequential/2, find_min_distance_parallel/2, find_min_distance_n_threads/3]).


%% "Variables":
map_size_x() -> 10*1000.
map_size_y() -> 10*1000.


%% "Private":
distance({X1, Y1}, {X2, Y2}) ->
  math:sqrt(
    math:pow(X1 - X2, 2) + math:pow(Y1 - Y2, 2)
  ).

get_n_random_points(N) ->
  [{rand:uniform(map_size_x()), rand:uniform(map_size_y())} || _ <- lists:seq(1, N)].


list_into_n_batches(List, N) ->
  Length = length(List),
  Batch_size = trunc(math:floor(Length / N)),
  New_list = lists:sublist(List, N * Batch_size),
  Remaining = [[El] || El <- lists:sublist(List, N * Batch_size+1, Length)] ++
    [[] || _ <-  lists:seq(1, N - (Length - N * Batch_size))],

  Batches = [lists:sublist(New_list, (X-1) * Batch_size + 1, Batch_size) || X <- lists:seq(1, N)],
  [Batch ++ Element || {Batch, Element} <- lists:zip(Batches, Remaining)].


%% "Public":
get_random_coordinates(Number_of_people, Number_of_parcels) ->
  {get_n_random_points(Number_of_people), get_n_random_points(Number_of_parcels)}.


%% Sequential computing
find_min_distance_sequential(People_locations, Parcels_locations) ->
  Distances = [{distance(P1, P2), {P1, P2}} || P1 <- People_locations, P2 <- Parcels_locations],
  lists:min(Distances).


%% Parallel computing
find_min_distance_parallel(People_locations, Parcels_locations) ->
  PID = self(),
  _Children =
    [spawn(fun() -> PID ! get_min_for_person(Person, Parcels_locations) end) || Person <- People_locations],
  Results =
    [receive Result -> Result end || _ <- People_locations],
  lists:min(Results).

get_min_for_person(Person, Parcels_locations) ->
  Distances = [{distance(Person, P), {Person, P}} || P <- Parcels_locations],
  lists:min(Distances).


%% N-thread computing
find_min_distance_n_threads(People_locations, Parcels_locations, Number_of_threads) ->
  Batches = list_into_n_batches(People_locations, Number_of_threads),
  PID = self(),
  _Children =
    [spawn(fun() -> PID ! find_min_distance_parallel(People_batch, Parcels_locations) end) || People_batch <- Batches],
  Results =
    [receive Result -> Result end || _ <- lists:seq(1, Number_of_threads)],
  lists:min(Results).


report() -> report(20*1000, 1*1000).
report(Number_of_people, Number_of_parcels) ->
  {People_locations, Parcels_locations} = get_random_coordinates(Number_of_people, Number_of_parcels),
  Arguments = [People_locations, Parcels_locations],
  partial_report(find_min_distance_sequential, Arguments),
  partial_report(find_min_distance_parallel, Arguments),
  partial_report(find_min_distance_n_threads, Arguments ++ [8]).

partial_report(Function, Arguments) ->
  {Time_micro_s, {Distance, {Closest_Person, Closest_Parcel}}} =
    timer:tc(?MODULE, Function, Arguments),
  io:format("Function ~w ~n", [Function]),
  io:format("Execution time: ~ws ~n", [Time_micro_s / math:pow(10, 6)]),
  io:format("Result: ~w ~n", [Distance]),
  io:format("Person: ~w;  Parcel: ~w ~n~n", [Closest_Person, Closest_Parcel]).


%%  Results:
%%
%%  Function find_min_distance_sequential
%%  Execution time: 10.187571s
%%  Result: 1.0
%%  Person: {5367,7819};  Parcel: {5366,7819}
%%
%%  Function find_min_distance_parallel
%%  Execution time: 1.569177s
%%  Result: 1.0
%%  Person: {5367,7819};  Parcel: {5366,7819}
%%
%%  Function find_min_distance_n_threads
%%  Execution time: 1.752371s
%%  Result: 1.0
%%  Person: {5367,7819};  Parcel: {5366,7819}
