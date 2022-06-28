-module(business_logic_test).
-include_lib("eunit/include/eunit.hrl").


setup() ->
    ok.

cleanup(_) ->
    ok.

main_test_() ->
    {inorder,
     {foreach,
      fun setup/0,
      fun cleanup/1,
      [fun trivial_test/1]
     }}.

trivial_test(_) ->
    fun () -> 
            ?assertEqual(1, 1)
    end.
