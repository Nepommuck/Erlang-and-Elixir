-module(fun_module).
-author("pawel").

%% API
-export([replace/1, count_div3/1, digit_sum/1]).


replace(List) ->
  Replace = fun
              F([]) -> [];
              F([$a | T]) -> [$e | F(T)]; F([$e | T]) -> [$o | F(T)];
              F([H | T]) -> [H | F(T)]
            end,
  Replace(List).


count_div3(L) ->
  Count_div3 = fun(List) -> length([X || X <- List, X rem 3 == 0])
               end,
  Count_div3(L).


digit_sum(Integer) ->

  Digit_sum = fun(Number) ->
    lists:foldl(
      fun(Char, Sum) -> Sum + (Char - $0) end, 0, integer_to_list(Number)
    )
              end,
  Digit_sum(Integer).
