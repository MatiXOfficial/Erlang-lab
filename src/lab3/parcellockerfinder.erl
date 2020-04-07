-module(parcellockerfinder).

-export([findMyParcelLocker/2, generateData/2, countSequential/2, countParallel/2, parallelFun/3,
         countLessParallel/2, lessParallelFun/3]).

findMyParcelLocker(_, []) ->
  {error, lockers_list_empty};
findMyParcelLocker({PX, PY}, [{LX, LY} | T]) ->
  CurrentDist = math:sqrt(math:pow(PX - LX, 2) + math:pow(PY - LY, 2)),
  case T of
    [] -> {{LX, LY}, CurrentDist};
    _ ->
      {{X, Y}, Dist} = findMyParcelLocker({PX, PY}, T),
      case CurrentDist < Dist of
        true -> {{LX, LY}, CurrentDist};
        false -> {{X, Y}, Dist}
      end
  end.

generateData(PeopleNum, LockersNum) ->
  Lockers = [{random:uniform(10001) - 1, random:uniform(10001) - 1} || _ <- lists:seq(1, LockersNum)],
  People = [{random:uniform(10001) - 1, random:uniform(10001) - 1} || _ <- lists:seq(1, PeopleNum)],
  {People, Lockers}.

%%%%% Wersja sekwencyjna %%%%%%%%%%%%%%

countSequential(People, Lockers) ->
  [{Person, findMyParcelLocker(Person, Lockers)} || Person <- People].

%%%%% Wersja równoległa %%%%%%%%%%%%%%%
countParallel(People, Lockers) ->
  SpawnFun = fun(Person) ->
    spawn(?MODULE, parallelFun, [Person, Lockers, self()]) end,
  lists:foreach(SpawnFun, People),
  collectData(length(People), []).

collectData(0, Results) ->
  Results;
collectData(N, Results) ->
  receive
    Result ->
      collectData(N - 1, [Result | Results])
  end.

parallelFun(Person, Lockers, Parent) ->
  Parent ! {Person, findMyParcelLocker(Person, Lockers)}.

%%%%% Wersja mniej równoległa %%%%%%%%
countLessParallel(People, Lockers) ->
  CoresNum = erlang:system_info(logical_processors_available),
  N = length(People) div CoresNum,
  PeopleList = [lists:sublist(People, S, N) || S <- lists:seq(1, length(People), N)],
  SpawnFun = fun(NewPeople) ->
    spawn(?MODULE, lessParallelFun, [NewPeople, Lockers, self()]) end,
  lists:foreach(SpawnFun, PeopleList),
  collectData(length(People), []).

lessParallelFun(People, Lockers, Parent) ->
  SendFun = fun(Person) -> Parent ! {Person, findMyParcelLocker(Person, Lockers)} end,
  lists:foreach(SendFun, People).