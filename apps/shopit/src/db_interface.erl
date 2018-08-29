-module(db_interface).

-export([setup_db/0, drop_my_tables_do_not_do_this/0]).

setup_db() ->
    application:stop(mnesia),
    mnesia:create_schema([node()]),
    application:start(mnesia),
    store:create_table(),
    users:create_table(),
    shopping_list:create_table(),
    item:create_table().

drop_my_tables_do_not_do_this() ->
    mnesia:delete_table(store),
    mnesia:delete_table(user),
    mnesia:delete_table(shopping_list),
    mnesia:delete_table(item).
