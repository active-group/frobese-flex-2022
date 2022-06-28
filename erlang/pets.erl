-module(pets).
-export([pets/1]).

-type pets_type() :: cat | snake | dog.
-spec pets(pets_type()) -> neidig | lieb.
pets(Pets_type) ->
    case Pets_type of
        cat -> 'neidig';
        snake -> 'neidig';
        dog -> 'lieb'
    end.
