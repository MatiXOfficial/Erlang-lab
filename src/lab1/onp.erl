-module(onp).

-export([onp/1]).

%funkcje string:to_integer, string:to_float

onp(text) ->
  onp(string:tokens(text, " "), []).

onp([], [Res]) ->
  Res;
onp(["+" | T1], [A, B | T2]) ->
  onp(T1, [A + B | T2]);
onp([H | T], S) ->
  onp(T, [H | S]).
