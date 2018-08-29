-module(users).

-include("shopit_common.hrl").

-export([ create_table/0
        , add/1
        , get/1
        , list/0]).

create_table() ->
    Res = mnesia:create_table( user,
                               [ {type, set},
                                 {disc_copies, [node()]},
                                 {index, []},
                                 {attributes, record_info(fields, user)}]),
    lager:info("Trying to create ~p table with result ~p", [?MODULE, Res]).

add(User) ->
    {atomic,_} = mnesia:transaction(fun() -> mnesia:write(User) end),
    User.

get(Uuid) ->
    {atomic, [User]} = mnesia:transaction(fun() -> mnesia:read({user,Uuid}) end),
    User.

list() ->
    ets:tab2list(user).
