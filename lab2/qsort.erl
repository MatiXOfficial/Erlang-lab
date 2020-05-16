-module(qsort).

-export([qs/1, randomElems/3, compareSpeeds/3]).

lessThan(List, Arg) -> [X || X <- List, X < Arg].

grtEqThan(List, Arg) -> [X || X <- List, X >= Arg].

qs([]) -> [];
qs([Pivot | Tail]) -> qs( lessThan(Tail, Pivot) ) ++ [Pivot] ++ qs( grtEqThan(Tail, Pivot) ).

randomElems(N, Min, Max) -> [Min + rand:uniform() * (Max - Min) || X <- lists:seq(1, N)].

compareSpeeds(List, Fun1, Fun2) ->
  {Time1, _} = timer:tc(Fun1, [List]),
  {Time2, _} = timer:tc(Fun2, [List]),
  Sig = if
          Time1 > Time2 -> ">";
          Time1 == Time2 -> "==";
          true -> "<"
        end,

  io:format("1: ~B ~s 2: ~B~n", [Time1, Sig, Time2]).