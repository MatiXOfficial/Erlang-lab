-module(pollution).

-export([createMonitor/0, addStation/3, addValue/5, removeValue/4, getOneValue/4, getStationMean/3, getDayMean/3, getTypeCount/1, getDaySummary/2]).

-record(reading, {date, what, value}).  % Dane odczytu
-record(station, {name, coords}). % Dane stacji

% Tworzy monitor jako zwykłą mapę.
createMonitor() -> #{}.

% Dodaje stację jako klucz istniejącego monitora (mapy)
addStation(Name, {X, Y}, M) ->
  case checkStation(Name, {X, Y}, maps:keys(M)) of
    ok -> M#{#station{name=Name, coords={X, Y}} => []};
    {error, Mes} -> {error, Mes}
  end.

% Dodaje odczyt do stacji istniejącej już w monitorze
addValue(Arg, Date, What, Value, M) ->
  case getStationInfo(Arg, M) of
    {error, Mes} -> {error, Mes};
    Station ->
      Values = maps:get(Station, M),
      case checkValue(Date, What, Values) of
        ok -> M#{Station := [#reading{date=Date, what=What, value=Value} | Values]};
        {error, Mes} -> {error, Mes}
      end
  end.

% Usuwa odczyt ze stacji (po współrzędnych albo nazwie)
removeValue(Arg, Date, What, M) ->
  case getStationInfo(Arg, M) of
    {error, Mes} -> {error, Mes};
    Station ->
      Values = maps:get(Station, M),
      case checkValue(Date, What, Values) of
        ok -> {error, no_such_a_value_in_the_monitor};
        {error, {_, Value}} ->
          M#{Station := Values -- [#reading{date=Date, what=What, value=Value}]}
      end
  end.

% Zwraca wartość pomiaru z danej stacji o danej godzinie i typie
getOneValue(Arg, Date, What, M) ->
  case getStationInfo(Arg, M) of
    {error, Mes} -> {error, Mes};
    Station ->
      Values = maps:get(Station, M),
      case checkValue(Date, What, Values) of
        ok -> {error, no_such_a_value_in_the_monitor};
        {error, {_, Value}} -> Value
      end
  end.

% Zwraca średnią wartość pomiaru danego typu z danej stacji
getStationMean(Arg, What, M) ->
  case getStationInfo(Arg, M) of
    {error, Mes} -> {error, Mes};
    Station ->
      listMean([Value || #reading{date=_, what=WhatL, value=Value} <- maps:get(Station, M), What == WhatL])
  end.

% Zwraca średnią wartość pomiaru danego typu, danego dnia {Rok, Miesiąc, Dzień} ze wszystkich stacji
getDayMean(What, Day, M) ->
  listMean([Value || #reading{date={DayL, _}, what=WhatL, value=Value} <- lists:flatten(maps:values(M)), What == WhatL, Day == DayL]).

% Zwraca mapę typów pomiarów i liczby ich wykonania we wszystkich stacjach
getTypeCount(M) ->
  listCountDistinct([What || #reading{date=_, what=What, value=_} <- lists:flatten(maps:values(M))]).

% Zwraca mapę typów pomiarów i ich średniej wartości dla danego dnia
getDaySummary(Day, M) ->
  TypesTmp = [What || #reading{date={DayL, _}, what=What, value=_} <- lists:flatten(maps:values(M)), Day == DayL],
  Types = sets:to_list(sets:from_list(TypesTmp)),
  [{What, getDayMean(What, Day, M)} || What <- Types].


%%%%%%%%%%%% Funkcje pomocnicze %%%%%%%%%%%%%%%%%%%%%%
% Sprawdza czy nazwa lub współrzędne znajdują się już w monitorze
checkStation(Name, Coords, M) when is_map(M) ->
  checkStation(Name, Coords, maps:keys(M));
checkStation(_, _, []) ->
  ok;
checkStation(Name, {X, Y}, [#station{name=Name, coords={X, Y}} | _]) ->
  {error, station_already_in_the_monitor};
checkStation(Name, _, [#station{name=Name, coords=_} | _]) ->
  {error, name_already_in_the_monitor};
checkStation(_, {X, Y}, [#station{name=_, coords={X, Y}} | _]) ->
  {error, coords_already_in_the_monitor};
checkStation(Name, {X, Y}, [_ | T]) ->
  checkStation(Name, {X, Y}, T).

% Na podstawie współrzędnych albo nazwy zwraca obie informacje o stacji
getStationInfo(Arg, M) when is_map(M) ->
  getStationInfo(Arg, maps:keys(M));
getStationInfo(_, []) ->
  {error, no_such_a_station_in_the_monitor};
getStationInfo({X, Y}, [#station{name=Name, coords={X, Y}} | _]) ->
  #station{name=Name, coords={X, Y}};
getStationInfo(Name, [#station{name=Name, coords={X, Y}} | _]) ->
  #station{name=Name, coords={X, Y}};
getStationInfo(Arg, [_ | T]) ->
  getStationInfo(Arg, T).

% Sprawdza czy do monitora można dodać pomiar
checkValue(Date, What, M) when is_map(M) ->
  checkValue(Date, What, maps:keys(M));
checkValue(_, _, []) ->
  ok;
checkValue(Date, What, [#reading{date=Date, what=What, value=Value} | _]) ->
  {error, {value_already_in_the_monitor, Value}};
checkValue(Date, What, [_ | T]) ->
  checkValue(Date, What, T).

% Liczy liczbę elementów w danej liście
countList([]) -> 0;
countList([_ | T]) ->
  1 + countList(T).

% Liczy średnią z elementów w danej liście
listMean([]) -> 0;
listMean(L) -> lists:sum(L) / countList(L).

% Liczy liczbę wystąpień różnych elementów listy i zwraca wynik jako mapę
listCountDistinct([]) -> #{};
listCountDistinct([H | T]) ->
  M = listCountDistinct(T),
  case maps:is_key(H, M) of
    true -> M#{H => maps:get(H, M) + 1};
    false -> M#{H => 1}
  end.