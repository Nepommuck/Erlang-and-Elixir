-module(qsort).
-author("pawel").

%% API
-export([qs/1, random_elems/3, compare_speeds/3]).


less_than(List, Arg) -> [X || X <- List, X < Arg].

grt_eq_than(List, Arg) -> [X || X <- List, X >= Arg].

qs([]) -> [];
qs([Head | []]) -> [Head];
qs([Pivot | Tail]) -> qs( less_than(Tail,Pivot) ) ++ [Pivot] ++ qs( grt_eq_than(Tail,Pivot) ).


random_elems(N, Min, Max) -> [Min - 1 + rand:uniform(Max - Min + 1) || _ <- lists:seq(1, N)].


compare_speeds(List, Fun1, Fun2) ->
  {Time1, _} = timer:tc(Fun1, [List]),
  {Time2, _} = timer:tc(Fun2, [List]),
  MicroSeconds_To_Seconds = 1/(1000*1000),
  {Time1 * MicroSeconds_To_Seconds, Time2 * MicroSeconds_To_Seconds}.


%% To execute in shell:
%% $ qsort:compare_speeds(qsort:random_elems(1000*1000, 0, 5000*1000), fun lists:sort/1, fun qsort:qs/1).
