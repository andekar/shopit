-module(item).

-include("shopit_common.hrl").

-export([ create_table/0
        , add/1
        , list/0]).

create_table() ->
    Res = mnesia:create_table( item,
                               [ {type, set},
                                 {disc_copies, [node()]},
                                 {index, []},
                                 {attributes, record_info(fields, item)}]),
    lager:info("Trying to create ~p table with result ~p", [?MODULE, Res]).

add(Item) ->
    {atomic,_} = mnesia:transaction(fun() -> mnesia:write(Item) end),
    Item.

list() ->
    ets:tab2list(item).
