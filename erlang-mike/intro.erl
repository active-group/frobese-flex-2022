-module(intro).
-export([double/1, double/2, state/1, typical/1, dogs_per_leg/1,
         t1/0, t2/0, minutes_since_midnight/1,
         d1/0, d2/0, p1/0, p2/0, run_over_dillo/1, run_over_animal/1,
         list_sum/1, feed_animal/2, feed_animals/2, highway/0,
         rev/1, list_map/2,
         start_process/0, process/0,
         counter/1, get_counter/1, counter_add/2]).
-compile([{nowarn_unused_function, [foo/0]}]).

-include_lib("eunit/include/")
% double/1 die einstellige Funktion double

% ^^^  sollte dem Dateinamen entsprechen
% 
% Alles mit Punkt abschließen
% 42 Zahl
% mike Atom (Kleinbuchstabe)
% "mike" (String)
% {1, "mike", sperber} Tupel
% <<1,2,3>> Binary: Bytes 1 2 3
% <<"mike">> Binary, mit ASCII-Codierung
% [1,2,3] Liste
% Anmerkung: Strings sind Listen
% #{a => "hello"}. Map
% #{a => "hello", b => "goodbye"}.
% Variablen fangen mit Großbuchstaben an
% 
% Liste ist eins der folgenden:
% - die leere Liste
% - eine Cons-Liste aus erstem Element und Rest
-spec double(number()) -> number().
double(X) -> X * 2.

-spec double(number(), number()) -> number(). 
double(X, Y) -> double(X * Y).

-type st() :: fest | fluessig | gas.

% Aggregatzustand von Wasser
-spec state(number()) -> st().
state(Temp) ->
    if 
        Temp < 0 -> fest;
        Temp > 100 -> gas; % ; dazwischen
        true -> fluessig
    end.

% fest | fluessig | gas: Fallunterscheidung
% Eine Funktionsklausel pro Fall. 

% Typische Temperatur für Aggregatzustand
-spec typical(st()) -> number() | error.
% Pattern Matching, Reihenfolge wichtig

% typical(fest) -> -20;
% typical(fluessig) -> 20;
% typical(gas) -> 100.
% 
typical(State) ->
    case State of
        fest -> -20;
        fluessig -> 20;
        gas -> 100;
        _ -> error
    end.

-spec safe_divide(number(), number()) -> {error, divide_by_zero} | {ok, number()}.
safe_divide(X, Y) ->
    if
        Y == 0 -> {error, divide_by_zero};
        true -> {ok, X / Y}
    end.

dogs_per_leg(Legs) ->
    case safe_divide(Legs, 0) of
        {error, Message} -> 
            io:format("problem: ~w~n", [Message]), % Komma!
            {error, bug_in_code};
        {ok, Dogs} -> Dogs
    end.

% 3 Sorten Haustiere:
% Hund, Katze, Schlange
% Schreibe eine Funktion, die liefert, ob ein Haustier niedlich ist.

% Eine Uhrzeit besteht aus:
% - Stunde
% - Minute
% zusammengesetzte Daten
-record(time, {hour :: 0..23, minute :: 0..59}).

t1() -> #time{ hour = 11, minute = 34}.
t2() -> #time{ hour = 12, minute = 12}.

% Minuten seit Mitternacht
-spec minutes_since_midnight(#time{}) -> non_neg_integer().
minutes_since_midnight(#time{ hour = H, minute = M }) ->
    HM = H * 60,
    HM + M.

% Ein Gürteltier hat folgende Eigenschaften:
% - tot oder lebendig - UND -
% - Gewicht
% zusammengesetzte Daten
-record(dillo, { liveness :: dead | alive, weight :: number() }).

% Ein Papagei hat folgende Eigenschaften:
% - Satz - UND -
% - Gewicht
-record(parrot, { sentence :: string(), weight :: number() }).

% Ein Tier ist eins der folgenden:
% - Gürteltier - ODER -
% - Papagei
% gemischte Daten
-type animal() :: #dillo{} | #parrot{}.

d1() -> #dillo { liveness = alive, weight = 10 }.
d2() -> #dillo { liveness = dead, weight = 8 }.
p1() -> #parrot { sentence = "Hello!", weight = 1 }.
p2() -> #parrot { sentence = "Goodbye!", weight = 1.5 }.

% Gürteltier überfahren
-spec run_over_dillo(#dillo{}) -> #dillo{}.
% run_over_dillo(#dillo{ weight = Weight}) ->
%     #dillo{ liveness = dead, weight = Weight}.
% run_over_dillo(D) -> D#dillo{ liveness = dead}.    
run_over_dillo(D) ->
    #dillo { liveness = dead, weight = D#dillo.weight }.
    
-spec run_over_animal(animal()) -> animal().
run_over_animal(#dillo{} = D) ->
    D#dillo { liveness = dead};
run_over_animal(#parrot{ weight = Weight }) ->
    #parrot { sentence = "", weight = Weight}.

foo() -> bar.

% Gürteltier füttern

% Eine Liste eins der folgenden:
% - die leere Liste                                   []
% - eine Cons-Liste aus erstem Element und Rest-Liste [First | Rest]
%                                               ^^^^^ Selbstbezug
-spec list_sum(list(number())) -> number().
list_sum([]) -> 0;
list_sum([First | Rest]) -> 
    First + list_sum(Rest).

highway() -> [d1(), d2(), p1(), p2()].

-spec feed_animal(animal(), number()) -> animal().
feed_animal(#dillo{liveness=alive, weight = Weight}, Amount) ->
    #dillo { liveness = alive, weight = Weight + Amount};
feed_animal(#dillo{liveness=dead} = D, _) -> D;
feed_animal(#parrot{ weight = Weight} = P, Amount) ->
   P#parrot { weight = Weight + Amount}.


% feed_animals([], _) ->[];
% feed_animals([First|Rest], Amount) ->
%    [feed_animal(First, Amount) | feed_animals(Rest, Amount)].
feed_animals(List, Amount) ->
    list_map(fun (Animal) -> intro:feed_animal(Animal, Amount) end, List).

list_map(_, []) ->[];
list_map(F, [First|Rest]) ->
    [F(First) | list_map(F, Rest)].

% Liste umdrehen
-spec rev(list(A)) -> list(A).
% quadratisch
% rev([]) -> [];
% rev([First | Rest]) -> rev(Rest) ++ [First].
% Acc ist die Liste der bisher gesehenen Elemente, umgedreht.
rev(List) -> rev(List, []).
rev([], Acc) -> Acc;
rev([First|Rest], Acc) ->
  % tail call, verbraucht keinen Speicher auf dem "Stack"
  rev(Rest, [First | Acc]).


start_process() ->
    % spawn(fun process/0)
    % spawn(intro, process)
    spawn(?MODULE, process). % die Funktion process im Modul intro

process() ->
    receive % wie case
        Message ->
            io:format("received message: ~w~n", [Message]),
            process()
    end.

% "Supervisor"
-spec counter(number()) -> pid().
counter(Start) ->
    process_flag(trap_exit, true),
    % spawn + link atomar
    Pid = spawn_link(fun () -> counter_loop(Start) end),
    register(counter_service, Pid),
    % link(Pid), % "Dein Schicksal ist mein Schicksal"
    % Wenn ein gelinkter Prozeß stirbt, bekommen wir 
    % stattdessen eine Nachricht.
    receive
        {'EXIT', _FromPid, _Reason} ->
            counter(Start)
    end.

get_counter(Pid) ->
    % RPC
    Pid ! {get, self()},
    receive
        Value -> Value
        after 5000 ->
            timeout
    end.

-spec counter_add(pid(), number()) -> ok.
counter_add(Pid, Inc) ->
    Pid ! Inc,
    ok.

counter_loop(N) ->
    receive
        {get, SenderPid} ->
            SenderPid ! N,
            counter_loop(N);
        Inc ->
            io:format("at counter ~w received inc ~w~n", [N, Inc]),
            counter_loop(N + Inc)
    end.

% Zahlen, Atome, Strings, Binaries, Listen, Tupel, Maps: "serialisieren sich selbst"
% "Term", typespec term()