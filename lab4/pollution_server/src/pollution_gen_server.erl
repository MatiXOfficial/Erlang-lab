-module(pollution_gen_server).
-behaviour(gen_server).

-export([start/0, stop/0, getMonitor/0, addStation/2, addValue/4, removeValue/3, getOneValue/3, getStationMean/2, getDayMean/2, getTypeCount/0, getDaySummary/1, crash/0]).
-export([init/1, handle_call/3, handle_cast/2,  terminate/2]).


%%%%%%%% START %%%%%%%%%%%%%%%%
start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, pollution:createMonitor(), []).

init(M) -> {ok, M}.

%%%%%%% INTERFACE %%%%%%%%%%%%%
stop() -> gen_server:call(?MODULE, terminate).

getMonitor() -> gen_server:call(?MODULE, getMonitor).
addStation(Name, {X, Y}) -> gen_server:call(?MODULE, {addStation, {Name, {X, Y}}}).
addValue(Arg, Date, What, Value) -> gen_server:call(?MODULE, {addValue, {Arg, Date, What, Value}}).
removeValue(Arg, Date, What) -> gen_server:call(?MODULE, {removeValue, {Arg, Date, What}}).
getOneValue(Arg, Date, What) -> gen_server:call(?MODULE, {getOneValue, {Arg, Date, What}}).
getStationMean(Arg, What) -> gen_server:call(?MODULE, {getStationMean, {Arg, What}}).
getDayMean(What, Day) -> gen_server:call(?MODULE, {getDayMean, {What, Day}}).
getTypeCount() -> gen_server:call(?MODULE, {getTypeCount, {}}).
getDaySummary(Day) -> gen_server:call(?MODULE, {getDaySummary, {Day}}).

%% Funkcja crashująca %%
crash() -> gen_server:cast(?MODULE, crash).
%% Po jej wykonaniu serwer przestaje działać.
%% Podobnie dzieje się po dodaniu pollution_gen_server do listy dzieci supervisora.
%% Jest to spowodowane tym, że intensity w SupFlags w supervisorze jest ustawione na 0.
%% Wystarczy zmienić więc intensity na np. 1 i serwer się podniesie.
%% Przy aktulnych ustawieniach (intensity = 1, period = 1) nie serwer zresetuje się maksymalnie raz na sekundę.
%% Zbyt częste używanie funkcji crash sprawi, że serwer przestanie działać.
%% W przypadku awarii serwera, dane zostaną utracone, tzn. monitor będzie pusty.
%% Problem ten można rozwiązać używając funkcji terminate do zapisywania stanu monitora do pliku w celu odtworzenia przy restarcie.

%%%%%%% MESSAGE HANDLING %%%%%%
handle_call(getMonitor, _From, M) -> {reply, M, M};
handle_call({addStation, {Name, {X, Y}}}, _From, M) -> handleHelper(M, {new, pollution:addStation(Name, {X, Y}, M)});
handle_call({addValue, {Arg, Date, What, Value}}, _From, M) -> handleHelper(M, {new, pollution:addValue(Arg, Date, What, Value, M)});
handle_call({removeValue, {Arg, Date, What}}, _From, M) -> handleHelper(M, {new, pollution:removeValue(Arg, Date, What, M)});
handle_call({getOneValue, {Arg, Date, What}}, _From, M) -> handleHelper(M, {res, pollution:getOneValue(Arg, Date, What, M)});
handle_call({getStationMean, {Arg, What}}, _From, M) -> handleHelper(M, {res, pollution:getStationMean(Arg, What, M)});
handle_call({getDayMean, {What, Day}}, _From, M) -> handleHelper(M, {res, pollution:getDayMean(What, Day, M)});
handle_call({getTypeCount, {}}, _From, M) -> handleHelper(M, {res, pollution:getTypeCount(M)});
handle_call({getDaySummary, {Day}}, _From, M) -> handleHelper(M, {res, pollution:getDaySummary(Day, M)});
handle_call(terminate, _From, M) -> {stop, normal, ok, M}.

handle_cast(crash, M) -> no:exist(), {noreply, M}.

terminate(normal, _) -> io:format("=========== terminating =============== ~n"), ok.

handleHelper(OldM, {_, {error, Mes}}) ->
  {reply, {error, Mes}, OldM};
handleHelper(_, {new, NewM}) ->
  {reply, ok, NewM};
handleHelper(M, {res, Res}) ->
  {reply, Res, M}.