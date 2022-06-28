%%%-------------------------------------------------------------------
%% @doc erlbank_monolithic public API
%% @end
%%%-------------------------------------------------------------------

-module(erlbank_monolithic_app).

-behaviour(application).

-export([start/2, stop/1]).




start_cowboy() ->
    %% Cowboy test code
    Dispatch = cowboy_router:compile([{'_', [{"/", web_frontend, index},
                                             {"/accounts/open", web_frontend, open_account},
                                             {"/transfers/create", web_frontend, create_transfer},
                                             {"/bank-statements/request", web_frontend, request_bank_statement}]}]),

    {ok, _} = cowboy:start_clear(my_http_listener,
                                 [{port, 8000}],
                                 #{env => #{dispatch => Dispatch}}).


start(_StartType, _StartArgs) ->
    database:init_database(),
    start_cowboy(),
    erlbank_monolithic_sup:start_link().

stop(_State) ->
    ok.

