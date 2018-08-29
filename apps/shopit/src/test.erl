-module(test).
-compile(export_all).

-include("shopit_common.hrl").

test_item1() -> #item{language = <<"SE">>, name = <<"mjolk">>}.
test_item2() -> #item{language = <<"SE">>, name = <<"kaffe">>}.
test_item3() -> #item{language = <<"SE">>, name = <<"apelsin">>}.

test_items() -> [test_item1, test_item2, test_item3].

shopping_list_item1() -> #shopping_list_item{ item = <<"mjolk">>,
                                              added_by = <<"anders">>,
                                              comment = <<"WE REALLY NEED IT">>,
                                              quantity = 3,
                                              quantity_left = 2
                                            }.
shopping_list_item2() -> #shopping_list_item{ item = <<"kaffe">>,
                                              added_by = <<"anders">>,
                                              comment = <<"WE REALLY NEED IT">>,
                                              quantity = 2,
                                              quantity_left = 2
                                            }.
shopping_list_item3() -> #shopping_list_item{ item = <<"apelsin">>,
                                              added_by = <<"anders">>,
                                              comment = <<"WE REALLY NEED IT">>,
                                              quantity = 7,
                                              quantity_left = 5
                                            }.

empty_shopping_list() -> #shopping_list{name = <<"my first list">>,
                                        items =[]}.

shopping_list() -> #shopping_list{name = <<"my first list">>,
                                  items = [ shopping_list_item1()
                                          , shopping_list_item2()
                                          , shopping_list_item3()]}.

user1(Uuid) -> #user{ name = <<"anders">>
                , shopping_lists = [{uuid, Uuid}]}.

add_test_data_to_db() ->
    _Item1 = item:add(test_item1()),
    _Item2 = item:add(test_item2()),
    _Item3 = item:add(test_item3()),
    ShoppingList = shopping_list:add(shopping_list()),
    User = users:add(user1(ShoppingList#shopping_list.uuid)),
    User.

test_to_json() ->
    User = user1(<<"123">>),
    lager:info("adding user ~p", [User]),
    to_json(User#user{shopping_lists = [#uuid{}]}).
