-module(web_frontend).
-export([init/2]).


success() ->
    << "
      <p> Account with account number ~p was opened successfully </p> ~n
      <p> It could take several minutes until the account is ready for transfers </p>
      <a href=\"/\"> Back </a>
    " >>.


form() ->
    << "
<h3> Open Account </h3>
<form method=\"post\" action=\"/accounts/open\">
  <label for=\"accounts_firstname\"> Firstname </label>
  <input type=\"text\" id=\"accounts_firstname\" name=\"accounts_firstname\" />

  <label for=\"accounts_secondname\"> Secondname </label>
  <input type=\"text\" id=\"accounts_secondname\" name=\"accounts_secondname\" />

  <input type=\"submit\" value=\"Open account\" />
</form>" >>.



init(Req, add) ->

    lager:info("Creating new account"),

    {ok, KeyValuesL, _} = cowboy_req:read_urlencoded_body(Req),

    KeyValues = maps:from_list(KeyValuesL),
    Firstname = maps:get(<<"accounts_firstname">>, KeyValues),
    Secondname = maps:get(<<"accounts_secondname">>, KeyValues),

    % dispatch
    AccountNumber = 1,
    Body = io_lib:format(success(), [AccountNumber]),

    Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Body, Req),

    lager:info("Created account with account number ~p", [AccountNumber]),

    {ok, Req2, []};

init(Req0, index) ->
    ResponseBody = form(),
    Req = cowboy_req:reply(200,
                           #{<<"content-type">> => <<"text/html">>},
                           ResponseBody,
                           Req0),
    {ok, Req, []}.
