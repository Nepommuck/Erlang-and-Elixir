-module(myLists).
-author("pawel").

-export([contains/2, duplicateElements/1, sumFloats/1, tailSumFloats/1, accSumFloats/2]).


contains([], _) -> false;
contains([Value | _], Value) -> true;
contains([_ | Tail], Value) -> contains(Tail, Value).


duplicateElements([]) -> [];
duplicateElements([Head | Tail]) -> [Head, Head] ++ duplicateElements(Tail).


sumFloats([]) -> 0.0;
sumFloats([Head | Tail]) -> Head + tailSumFloats(Tail).


tailSumFloats(List) -> accSumFloats(List, 0.0).

accSumFloats([], Sum) -> Sum;
accSumFloats([Head | Tail], Sum) -> accSumFloats(Tail, Head + Sum).
