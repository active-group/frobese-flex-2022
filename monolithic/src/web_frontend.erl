
-module(web_frontend).
-include("data.hrl").
-export([init/2]).



account_opened_success() ->
    << "
      <p> Account with account number ~p was opened successfully </p> ~n
       <a href=\"/\"> Back </a>
    " >>.


account_open_form() ->
            << "
<h3> Open Account </h3>
               <form method=\"post\" action=\"/accounts/open\">
  <label for=\"accounts_firstname\"> Firstname </label>
  <input type=\"text\" id=\"accounts_firstname\" name=\"accounts_firstname\" />

  <label for=\"accounts_secondname\"> Secondname </label>
  <input type=\"text\" id=\"accounts_secondname\" name=\"accounts_secondname\" />

  <input type=\"submit\" value=\"Open account\" />
</form>" >>.


-spec bin_to_int(binary()) -> integer().
bin_to_int(B) ->
    erlang:list_to_integer(binary:bin_to_list(B)).

-spec transfer_error() -> binary().
transfer_error() ->
    << "
      <p> An error occured: ~p </p> ~n
       <a href=\"/\"> Back </a>
    " >>.

-spec transfer_success() -> binary().
transfer_success() ->
            << "
      <p> Transfer with id ~p successfully created </p> ~n
               <a href=\"/\"> Back </a>
    " >>.


transfer_form() ->
                    << "
<h3> Create transfer </h3>
                       <form method=\"post\" action=\"/transfers/create\">
  <label for=\"transfers_from\"> From (account number) </label>
  <input type=\"text\" id=\"transfers_from\" name=\"transfers_from\" />

  <label for=\"transfers_to\"> To (account number) </label>
  <input type=\"text\" id=\"transfers_to\" name=\"transfers_to\" />

  <label for=\"transfers_amount\"> Amount </label>
  <input type=\"text\" id=\"transfers_amount\" name=\"transfers_amount\" />

  <input type=\"submit\" value=\"Create transfer\" />
</form>" >>.



bank_statement_form() ->
    << "
<h3> Request bank-statement </h3>
  <form method=\"post\" action=\"/bank-statements/request\">
  <table>
       <tr>
       <td>
         <label for=\"bank_statement_accountnumber\"> Account number </label>
       </td>
       <td>
         <input type=\"text\" id=\"bank_statement_accountnumber\" 
                name=\"bank_statement_accountnumber\" />
       </td>
       </tr>

       <tr>
       <td>
         <label for=\"bank_statement_currency\"> Currency </label>
       </td>
       <td>
         <input type=\"text\" id=\"bank_statement_currency\" name=\"bank_statement_currency\"
                value=\"EUR\" size=3/>
       </td>
       </tr>

       <tr>
       <td>
         <label for=\"bank_statement_format\"> Number & date formatting </label>
       </td>
       <td>
         <select id=\"bank_statement_format\" name=\"bank_statement_format\">
           <option value=\"de\" selected>DE</option>
           <option value=\"en\">EN</option>
         </select>
       </td>
       </tr>
   </table>

  <input type=\"submit\" value=\"Request bank-statement\" />
</form>" >>.


-spec tx_template() -> string().
tx_template() ->
    "<tr>
      <td> ~p </td>
      <td> ~s </td>
      <td> ~s </td>
      <td> ~s </td>
      <td> ~s </td>
    </tr>".

-spec amount_to_string(money(), string(), number_formatter:locale()) -> string().
amount_to_string(Amount, Currency, Format) ->
    {ok, AmountExchanged} = exchange_service:exchange(Currency, Amount),
    AmountFormatted = number_formatter:format(Format, AmountExchanged),
    AmountFormatted++ " " ++ Currency.


%% returns the name of the person associated to the account nr
%% given by account number.
-spec name_by_account_nr(unique_id()) -> string().
name_by_account_nr(AccountNr) ->
    {ok, Account} = business_logic:get_account(AccountNr),
    name_by_account(Account).

%% returns the name of the person associated to the account
%% given by account.
-spec name_by_account(#account{}) -> string().
name_by_account(Account) ->
    {ok, Person}  = business_logic:get_person(Account#account.person_id),
    io_lib:format("~s ~s", [Person#person.firstname, Person#person.surname]).

-spec tx(#transfer{}, string(), number_formatter:locale()) -> string().
tx(Tx, Currency, Format) ->
    Name1 = name_by_account_nr(Tx#transfer.from_acc_nr),
    Name2 = name_by_account_nr(Tx#transfer.to_acc_nr),
    Amount = amount_to_string(Tx#transfer.amount, Currency, Format),
    Date = date_formatter:format(Format, Tx#transfer.timestamp),
    Id = Tx#transfer.id,
    io_lib:format(tx_template(), [Id, Date, Amount, Name1, Name2]).

head_template() ->
    "<p> Name: ~s </p>
     <p> Balance: ~s </p>
     <table>
      <tr>
        <th>ID</th>
        <th>Date</th>
        <th>Amount</th>
        <th>Sender</th>
        <th>Receiver</th>
      </tr> ".

back_button() ->
    "<a href=\"/\">Back </a>".

footer_template() ->
    "</table>" ++ back_button().


-spec head(#account{}, string(), number_formatter:locale()) -> string().
head(Account, Currency, Format) ->
    Amount = amount_to_string(Account#account.amount, Currency, Format),
    Name =  name_by_account(Account),
    io_lib:format(head_template(), [Name, Amount]).

-spec statement(#account{}, list(#transfer{}), string(), number_formatter:locale()) -> string().
statement(Account, Txs, Currency, Format) ->
    % TODO append all txs
    TxsString = lists:foldl(fun(Tx, Acc) -> Acc ++ tx(Tx, Currency, Format) end, "", Txs),
    io_lib:format("~s ~s ~s", [head(Account, Currency, Format), TxsString, footer_template()]).


index() ->
    io_lib:format("~s~s~s",
                  [account_open_form(),
                   transfer_form(),
                   bank_statement_form()]).

%% /bank-statements/request
init(Req, request_bank_statement) ->

    {ok, KeyValuesL, _} = cowboy_req:read_urlencoded_body(Req),

    KeyValues = maps:from_list(KeyValuesL),
    AccountNumber = bin_to_int(maps:get(<<"bank_statement_accountnumber">>, KeyValues)),
    Currency = binary_to_list(maps:get(<<"bank_statement_currency">>, KeyValues)),
    Format = list_to_atom(
               binary_to_list(maps:get(<<"bank_statement_format">>, KeyValues))),

    io:format("Account number ~p~n", [AccountNumber]),
    Ret = business_logic:get_account(AccountNumber),
    KnowsCurrency = exchange_service:knows_currency(Currency),
    case {KnowsCurrency, Ret} of
        {true, {ok, Account}} ->
            Txs = business_logic:get_transfers(AccountNumber),
            Body = statement(Account, Txs, Currency, Format),
            Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>},
                                    Body, Req),
            {ok, Req2, []};
        {false, _} ->
            Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>},
                                    "Currency not found.<br/>" ++ back_button(), Req),
            {ok, Req2, []};
        {_, {error, not_found}} ->
            Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>},
                                    "Account not found.<br/>" ++ back_button(), Req),
            {ok, Req2, []}
    end;


%% /transfers/create
init(Req, create_transfer) ->

    {ok, KeyValuesL, _} = cowboy_req:read_urlencoded_body(Req),

    KeyValues = maps:from_list(KeyValuesL),
    SenderAccountNumber =  bin_to_int(maps:get(<<"transfers_from">>, KeyValues)),
    ReceiverAccountNumber = bin_to_int(maps:get(<<"transfers_to">>, KeyValues)),
    Amount = bin_to_int(maps:get(<<"transfers_amount">>, KeyValues)),

    Body = case business_logic:transfer(SenderAccountNumber, ReceiverAccountNumber, Amount) of
               {ok, TxId} ->
                   io_lib:format(transfer_success(), [TxId]);
               {error, Err} ->
                   io_lib:format(transfer_error(), [Err])
           end,
    Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Body, Req),
    {ok, Req2, []};

%% /accounts/open
init(Req, open_account) ->

    lager:info("Creating new account"),

    {ok, KeyValuesL, _} = cowboy_req:read_urlencoded_body(Req),

    KeyValues = maps:from_list(KeyValuesL),
    Firstname = maps:get(<<"accounts_firstname">>, KeyValues),
    Secondname = maps:get(<<"accounts_secondname">>, KeyValues),

    Account = business_logic:open_account(Firstname, Secondname),
    Body = io_lib:format(account_opened_success(), [Account#account.account_number]),

    Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Body, Req),

    lager:info("Created account with account number ~p", [Account#account.account_number]),

    {ok, Req2, []};

%% /index
init(Req0, index) ->
    Req = cowboy_req:reply(200,
                           #{<<"content-type">> => <<"text/html">>},
                           index(),
                           Req0),
    {ok, Req, []}.
