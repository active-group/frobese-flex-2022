%% This module represents the database layer

-module(database).
-include("data.hrl").
-export([init_database/0, write/2, read_all/2,
         put_account/1, get_account/1, get_all_accounts/0,
         put_person/1, get_person/1, get_all_persons/0, 
         put_transfer/1, get_transfer/1, get_all_transfers/0, get_all_transfers/1, 
         unique_account_number/0,unique_tx_id/0, unique_person_id/0,
         atomically/1]).

close_tables() ->
    dets:close(person),
    dets:close(transfer),
    dets:close(account),
    dets:close(event),
    dets:close(table_id).

%% destroy tables in case they already existed
destroy_tables() ->
    file:delete("person.dets"),
    file:delete("transfer.dets"),
    file:delete("account.dets"),
    file:delete("event.dets"),
    file:delete("table_id.dets").

% unfortunately, delete_table doesn't always work such that create_table doesn't fail, so don't check return value
create_tables() ->
    {ok, person} = dets:open_file(person, [{type, set}, {file, "person.dets"}]),
    {ok, transfer} = dets:open_file(transfer, [{type, set}, {file, "transfer.dets"}]),
    {ok, account} = dets:open_file(account, [{type, set}, {file, "account.dets"}]),
    {ok, event} = dets:open_file(event, [{type, set}, {file, "event.dets"}]),
    {ok, table_id} = dets:open_file(table_id, [{type, set}, {file, "table_id.dets"}]),
    dets:insert(table_id, {person, 0}),
    dets:insert(table_id, {transfer, 0}),
    dets:insert(table_id, {account, 0}),
    dets:insert(table_id, {event, 0}).

init_database() ->
    close_tables(),
    destroy_tables(),
    create_tables(),
    ok.

write(Table, Tuple) ->
    ok = dets:insert(Table, Tuple),
    ok.

-spec read_one(dets:tab_name(), unique_id(), fun((tuple()) -> Obj)) -> {ok, Obj} | {error, not_found | more_than_one}.
read_one(Table, Id, Deserialize) ->
    Res = dets:lookup(Table, Id),
    case Res of
        [Tuple] -> {ok, Deserialize(Tuple)};
        []  -> {error, not_found};
        [_ | _] -> {error, more_than_one};
        Error -> Error
    end.

-spec read_all(dets:tab_name(), fun((tuple()) -> Obj)) -> list(Obj).
read_all(Table, Deserialize) ->
    Res = dets:select(Table,[{'_',[],['$_']}]),    
    lists:map(Deserialize, Res).

-spec put_account(#account{}) -> ok.
put_account(#account{account_number = AccountNumber, person_id = PersonId, amount = Amount}) ->
    write(account, {AccountNumber, PersonId, Amount}).

deserialize_account({AccountNumber, PersonId, Amount}) ->
    #account{account_number = AccountNumber, person_id = PersonId, amount = Amount}.

-spec get_account(account_number()) -> {ok, #account{}} | {error, any()}.
get_account(AccountNumber) ->
    read_one(account, AccountNumber, fun deserialize_account/1).

-spec get_all_accounts() -> list(#account{}).
get_all_accounts() -> read_all(account, fun deserialize_account/1).

-spec put_person(#person{}) -> ok.
put_person(#person{id = Id, firstname = Firstname, surname = Surname}) ->
    write(person, {Id, Firstname, Surname}).

deserialize_person({Id, Firstname, Surname}) ->
    #person{id = Id, firstname = Firstname, surname = Surname}.

-spec get_person(unique_id()) -> {ok, #person{} | {error, any()}}.
get_person(Id) ->
    read_one(person, Id, fun deserialize_person/1).

-spec get_all_persons() -> list(#person{}).
get_all_persons() -> read_all(person, fun deserialize_person/1).

-spec put_transfer(#transfer{}) -> ok.
put_transfer(#transfer{id = Id, timestamp = Timestamp, from_acc_nr = FromAccNr, to_acc_nr = ToAccNr, amount = Amount}) ->
    write(transfer, {Id, Timestamp, FromAccNr, ToAccNr, Amount}).

deserialize_transfer({Id, Timestamp, FromAccNr, ToAccNr, Amount}) ->
    #transfer{id = Id, timestamp = Timestamp, from_acc_nr = FromAccNr, to_acc_nr = ToAccNr, amount = Amount}.

-spec get_transfer(unique_id()) -> {ok, #transfer{}} | {error, any()}.
get_transfer(Id) ->
    read_one(transfer, Id, fun deserialize_transfer/1).

-spec get_all_transfers() -> list(#transfer{}).
get_all_transfers() -> read_all(transfer, fun deserialize_transfer/1).

-spec get_all_transfers(account_number()) -> list(#transfer{}).
get_all_transfers(AccountNr) ->
    Res = dets:select(transfer,
                           [{'$1',
                            [{'orelse',
                                {'==', {element, 3, '$1'}, AccountNr},
                                {'==', {element, 4, '$1'}, AccountNr}}],
                            ['$_']}]),
    lists:map(fun deserialize_transfer/1, Res).

-spec unique_account_number() -> unique_id().
unique_account_number() -> dets:update_counter(table_id, account, 1).

-spec unique_person_id() -> unique_id().
unique_person_id() -> dets:update_counter(table_id, person, 1).

-spec unique_tx_id() -> unique_id().
unique_tx_id() -> dets:update_counter(table_id, transfer, 1).

% holdover from Mnesia
-spec atomically(fun(() -> Ret)) -> Ret.
atomically(Fun) ->
    Fun().
