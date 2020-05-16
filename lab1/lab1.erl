-module(lab1).

-export([power/2]).


power(X, 0) ->
  1;
power(X, Y) when Y > 0 ->
  X * power(X, Y - 1);
power(X, Y) when Y < 0 ->
  1 / power(X, -Y).