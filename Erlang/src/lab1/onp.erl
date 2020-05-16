-module(onp).

-export([onp/1]).


onp(Text) ->
  onp(string:tokens(Text, " "), []).

onp([], [Res]) ->
  Res;
onp(["+" | T1], [A, B | T2]) ->
  onp(T1, [B + A | T2]);
onp(["-" | T1], [A, B | T2]) ->
  onp(T1, [B - A | T2]);
onp(["*" | T1], [A, B | T2]) ->
  onp(T1, [B * A | T2]);
onp(["/" | T1], [A, B | T2]) ->
  onp(T1, [B / A | T2]);

onp(["sqrt" | T1], [A | T2]) ->
  onp(T1, [math:sqrt(A) | T2]);
onp(["pow" | T1], [A, B | T2]) ->
  onp(T1, [math:pow(B, A) | T2]);
onp(["sin" | T1], [A | T2]) ->
  onp(T1, [math:sin(A) | T2]);
onp(["cos" | T1], [A | T2]) ->
  onp(T1, [math:cos(A) | T2]);
onp(["tg" | T1], [A | T2]) ->
  onp(T1, [math:tan(A) | T2]);
onp(["ctg" | T1], [A | T2]) ->
  onp(T1, [math:cos(A)/math:sin(A) | T2]);

onp(["-*+" | T1], [A, B | T2]) ->     %% a -*+ b = (a - b) * (a + b)
  onp(T1, [(B - A) * (B + A) | T2]);
onp(["rec" | T1], [A | T2]) ->        %% rec(a) = 1 / a
  onp(T1, [1 / A | T2]);

onp([H | T], S) ->
  case string:find(H, ".") == nomatch of
    true -> onp(T, [list_to_integer(H) | S]);
    false -> onp(T, [list_to_float(H) | S])
  end.