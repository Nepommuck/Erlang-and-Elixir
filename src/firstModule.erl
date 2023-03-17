-module(firstModule).
-author("pawel").

-export([factorial/1, power/2]).


factorial(0) -> 1;
factorial(N) -> N * factorial(N - 1).

power(_, 0) -> 1;
power(A, N) -> A * power(A, N - 1).
