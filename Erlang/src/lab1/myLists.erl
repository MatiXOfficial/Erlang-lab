-module(myLists).

-export([contains/2, duplicateElements/1, sumFloats/1, sumFloatsTail/2]).


contains([], Val) ->
  false;
contains([Val | _], Val) ->
  true;
contains([H | T], Val) ->
  contains(T, Val).


duplicateElements([]) ->
  [];
duplicateElements([H | T]) ->
  [H, H] ++ duplicateElements(T).


sumFloats([]) ->
  0;
sumFloats([H | T]) when is_float(H) ->
  H + sumFloats(T);
sumFloats([H | T]) ->
  sumFloats(T).


sumFloatsTail([], acc) ->
  acc;
sumFloatsTail([H | T], acc) when is_float(H) ->
  sumFloatsTail(T, acc + H);
sumFloatsTail([_, T], acc) ->
  sumFloatsTail(T, acc).