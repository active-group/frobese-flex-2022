-module(events_test).
-include_lib("eunit/include/eunit.hrl").
-include("events.hrl").

setup() ->
    database:init_database(),
    events:init_events().

cleanup(_) -> ok.

main_test_() ->
    {inorder,
     {foreach,
      fun setup/0,
      fun cleanup/1,
      [fun put_event/1]
     }}.

put_event(_) ->
    fun() ->
        Event1 = events:put_event(event1),
        Event2 = events:put_event(event2),
        ?assertEqual(events:get_all_events(), [Event1, Event2]),
        ?assertEqual(events:get_events_from(Event2#event.number), [Event2])
    end.
