%% This module represents the frontend layer

-module(client).
-include("data.hrl").
-export([open_account/2, transfer/3, bank_statement/1]).



%% returns the name of the person associated to the account 
%% given by account number.
-spec name_by_account_nr(account_number()) -> string().
name_by_account_nr(AccountNumber) ->
    {ok, Account} = business_logic:get_account(AccountNumber),
    {ok, Person}  = business_logic:get_person(Account#account.person_id),
    binary_to_list(Person#person.firstname) ++ " " ++ binary_to_list(Person#person.surname).


%% opens an acocunt with a given first and surname.
%% prints the result and the account number to stdout.
-spec open_account(string(), string()) -> ok.
open_account(Firstname, Surname) ->
    Account = business_logic:open_account(list_to_binary(Firstname), 
                                          list_to_binary(Surname)),
    io:format("Account was successfully opened. Account number: ~p ~n", [Account#account.account_number]).


%% transfers a given amount from the first account to the second account, identified
%% by their account number. Prints the transfer-id when successful, else the error
%% to stdout.
-spec transfer(account_number(), account_number(), money()) -> ok.
transfer(SenderAccountNumber, ReceiverAccountNumber, Amount) ->
    case business_logic:transfer(SenderAccountNumber, ReceiverAccountNumber, Amount) of
        {ok, TxId} ->
            io:format("Transfer successful, id: ~p~n", [TxId]);
        {error, Err} ->
            io:format("An error occured: ~p~n", [Err])
        end.



%% prints the header of a bank statement, namely the full name and the
%% current balance, associated with the account number to stdout.
print_head(AccountNumber) ->
    {ok, Account} = business_logic:get_account(AccountNumber),
    Name = name_by_account_nr(AccountNumber),
    io:format("~nBank statement for: ~s~n", [Name]),
    io:format("---------------------------------------------------- ~n", []),
    io:format("Balance: ~p~n", [Account#account.amount]),
    io:format("---------------------------------------------------- ~n", []).


%% takes an transfer record and prints it to stdout.
print_tx(Tx) ->
    Name1 = name_by_account_nr(Tx#transfer.from_acc_nr),
    Name2 = name_by_account_nr(Tx#transfer.to_acc_nr),
    Amount = Tx#transfer.amount,
    Id = Tx#transfer.id,
    io:format("#~p\t ~p\t ~s \t -> ~s ~n", [Id, Amount, Name1, Name2]).

%% takes a list of transfers records and prints them to stdout
print_txs(Txs) ->
    lists:map(fun print_tx/1, Txs).


%% takes an account number and prints a bank statement to stdout.
%% That is a full name, the current balance, and a list of
%% transfers associated with the account.
bank_statement(AccountNumber) ->
    Txs = business_logic:get_transfers(AccountNumber),
    SortedRelevantTxs = business_logic:sort_tx(Txs),

    print_head(AccountNumber),
    print_txs(SortedRelevantTxs),

    io:format("~n~n", []).
