-module(rpn).
-author("pawel").

-export([rpn/1, rpnCalc/2]).

%% 1) Result: 12.2
%% Infix: 1 + 2 * 3 - 4 / 5 + 6
%% RPN: "1 2 3 * + 4 5 / - 6 +"

%% 2) Result: 57
%% Infix: 1 + 2 + 3 + 4 + 5 + 6 * 7
%% RPN: "1 2 + 3 + 4 + 5 + 6 7 * +"

%% 3) Result: -62.(3)
%% Infix: ( (4 + 7) / 3 ) * (2 - 19)
%% RPN: "4 7 + 3 / 2 19 - *"

%% 4) Result: Division by 0.
%% Infix: 17 * (31 + 4) / ( (26 - 15) * 2 - 22 ) - 1
%% RPN: "17 31 4 + * 26 15 - 2 * 22 - / 1 -"


rpn(Str) ->
  List = string:tokens(Str, " "),
  rpnCalc(List, []).


%%rpnCalc(Operations: List, Stack: List) -> Number;
rpnCalc([], [Number]) -> Number;

rpnCalc(["+" | OperationTail], [Num1, Num2 | StackTail]) ->
  rpnCalc(OperationTail, [Num2 + Num1 | StackTail]);

rpnCalc(["-" | OperationTail], [Num1, Num2 | StackTail]) ->
  rpnCalc(OperationTail, [Num2 - Num1 | StackTail]);

rpnCalc(["*" | OperationTail], [Num1, Num2 | StackTail]) ->
  rpnCalc(OperationTail, [Num2 * Num1 | StackTail]);

rpnCalc(["/" | OperationTail], [Num1, Num2 | StackTail]) ->
  rpnCalc(OperationTail, [Num2 / Num1 | StackTail]);

rpnCalc(["^" | OperationTail], [Num1, Num2 | StackTail]) ->
  rpnCalc(OperationTail, [math:pow(Num2, Num1) | StackTail]);

rpnCalc(["sqrt" | OperationTail], [Num1 | StackTail]) ->
  rpnCalc(OperationTail, [math:sqrt(Num1) | StackTail]);

rpnCalc(["sin" | OperationTail], [Num1 | StackTail]) ->
  rpnCalc(OperationTail, [math:sin(Num1) | StackTail]);

rpnCalc(["cos" | OperationTail], [Num1 | StackTail]) ->
  rpnCalc(OperationTail, [math:cos(Num1) | StackTail]);

rpnCalc([Digits | OperationTail], Stack) ->
  rpnCalc(OperationTail, [string_to_number(Digits) | Stack]).


string_to_number(Str) ->
  case string:to_float(Str) of
    {error, no_float} -> list_to_integer(Str);
    {Float , _Rest} -> Float
  end.
