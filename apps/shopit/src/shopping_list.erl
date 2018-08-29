-module(shopping_list).

-include("shopit_common.hrl").

-export([ create_table/0
        , add/1
        , list/0]).

create_table() ->
    Res = mnesia:create_table( shopping_list,
                               [ {type, set},
                                 {disc_copies, [node()]},
                                 {index, []},
                                 {attributes, record_info(fields, shopping_list)}]),
    lager:info("Trying to create ~p table with result ~p", [?MODULE, Res]).

add(ShoppingList) ->
    {atomic,_} = mnesia:transaction(fun() -> mnesia:write(ShoppingList) end),
    ShoppingList.

list() ->
    ets:tab2list(shopping_list).
