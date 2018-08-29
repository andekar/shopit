-module(common).

%%-compile({parse_transform, ejson_trans}).
-include("shopit_common.hrl").

-export([get_timestamp/0, binary_uuid/0, test/1]).


test(Json) ->
    lager:info("WHAA ~p~n", [Json]),
    lager:info("~n~n~p~n~n", [to_json(test:shopping_list_item2())]),
    lager:info("~n~n~p~n~n", [to_json(test:empty_shopping_list())]),
    lager:info("~n~n~p~n~n", [to_json(test:shopping_list())]).
%    from_json(Json).

get_timestamp() ->
    {Mega, Seconds, Milli} = erlang:timestamp(),
    Mega * 1000000000000 + Seconds * 1000000 + Milli.

binary_uuid() ->
    list_to_binary(uuid:uuid_to_string(uuid:get_v4())).
