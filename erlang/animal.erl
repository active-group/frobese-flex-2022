-module(animal).
-export([run_over_animal/1,run_over_dillo/1,list_sum/1,d1/0,d2/0,p1/0,p2/0,highway/0,kill_all_animal/1,gt_feuttern/2]).

-record(dillo, { liveness :: dead | alive, weight :: number() }).
-record(parrot, { sentence :: string(), weight :: number() }).
-type animal() :: #dillo{} | #parrot{}.


d1() -> #dillo { liveness = alive, weight = 10 }.
d2() -> #dillo { liveness = dead, weight = 8 }.
p1() -> #parrot { sentence = "Hello!", weight = 1 }.
p2() -> #parrot { sentence = "Goodbye!", weight = 1.5 }.


-spec run_over_dillo(#dillo{}) -> #dillo{}.
run_over_dillo(D) ->
    #dillo { liveness = dead, weight = D#dillo.weight }.

-spec run_over_animal(animal()) -> animal().
run_over_animal(#dillo{} = D) ->
    D#dillo { liveness = dead};
run_over_animal(#parrot{ weight = Weight }) ->
    #parrot { sentence = "", weight = Weight}.

-spec list_sum(list(number())) -> number().
list_sum([]) -> 0;
list_sum([First | Rest]) -> 
    First + list_sum(Rest).



gt_feuttern(Food, #dillo{ liveness = Liveness, weight = W }) ->
    if
        Liveness /= dead -> {ok, #dillo{liveness = alive, weight = W + (Food * 0.2)}};
        Liveness == dead -> {ok, gt_is_dead}
    end.


highway() -> [d1(), d2(), p1(), p2()].

-spec kill_all_animal(list(animal())) -> list(animal()).
kill_all_animal([]) -> [];
kill_all_animal([F|R]) ->
    [run_over_animal(F)| kill_all_animal(R)].

