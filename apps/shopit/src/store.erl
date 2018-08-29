-module(store).

-include("shopit_common.hrl").

-export([ create_table/0
        , add/1
        , add/2
        , list/0]).

create_table() ->
    Res = mnesia:create_table( store,
                               [ {type, set},
                                 {disc_copies, [node()]},
                                 {index, []},
                                 {attributes, record_info(fields, store)}]),
    lager:info("Trying to create ~p table with result ~p", [?MODULE, Res]).

add(Store) ->
    {atomic,_} = mnesia:transaction(fun() -> mnesia:write(Store) end).

add(Name, {X,Y}) ->
    Store = #store{ name = Name
                  , x_coord = X
                  , y_coord = Y},
    {atomic,_} = mnesia:transaction(fun() -> mnesia:write(Store) end).

list() ->
    ets:tab2list(store).
