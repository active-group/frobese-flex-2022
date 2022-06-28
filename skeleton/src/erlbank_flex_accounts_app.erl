-module(erlbank_flex_accounts_app).
-behaviour(application).
-export([start/2, stop/1]).


start_cowboy() ->
    %% Cowboy test code
    Dispatch = cowboy_router:compile([{'_', [{"/accounts/", web_frontend, index},
                                             {"/accounts/open", web_frontend, add}]}]),
    {ok, _} = cowboy:start_clear(my_http_listener,
                                 [{port, 8000}],
                                 #{env => #{dispatch => Dispatch}}).


start(_StartType, _StartArgs) ->

    lager:info("Starting accounts-service: ~p~n", [node()]),

    start_cowboy(),
    %% database:init_database(),

    Res = erlbank_flex_accounts_sup:start_link(),
    lager:info("Started account feed: ~p~n", [node()]),
    Res.


stop(_State) ->
    %% database:destroy_tables().
    ok.

%% internal functions
