-type unique_id() :: integer().
-type account_number() :: integer().
-type money() :: number().

-record(person, 
    {id :: unique_id(), 
     firstname :: binary(),
     surname :: binary()}).
-record(account,
    {account_number :: account_number(),
     person_id :: unique_id(),
     amount :: money()}).
-record(transfer, 
    {id :: unique_id(), 
     timestamp :: erlang:timestamp(), 
     from_acc_nr :: account_number(), 
     to_acc_nr :: account_number(), 
     amount :: money()}).

