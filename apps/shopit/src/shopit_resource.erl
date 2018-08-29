-module(shopit_resource).
-export([content_types_provided/2, content_types_accepted/2,
         init/1, allowed_methods/2, from_json/2, to_html/2,
         delete_resource/2, delete_completed/2, process_post/2,
         resource_exists/2, malformed_request/2,
         service_available/2
        ]).

-include_lib("webmachine/include/webmachine.hrl").
-include("shopit_common.hrl").

%% Create a struct to put the json in
-define(JSONSTRUCT, struct).
-define(JSONSTRUCT(JSons), {?JSONSTRUCT, JSons}).

allowed_methods(ReqData, Context) ->
    {['GET', 'HEAD', 'PUT', 'DELETE', 'POST'], ReqData, Context}.

delete_resource(ReqData, Context) ->
    {false, ReqData, Context}.

delete_completed(ReqData, Context) ->
    {true, ReqData, Context}.

service_available(ReqData, Context) ->
    case ReqData#wm_reqdata.scheme of
        https ->
            {true, ReqData, Context};
        _ ->
            {true, ReqData, Context}
    end.

-spec init(list()) -> {ok, term()}.
init([]) ->
    {ok, []}.

from_json(ReqData, Context) ->
    Method = ReqData#wm_reqdata.method,
    Url = wrq:path_tokens(ReqData),
    [{json, Decoded}] = proplists:lookup_all(json, Context),
    [{userdata, Ud}] = proplists:lookup_all(userdata, Context),
    SReply = try
                 mochijson2:encode([[{error,request_failed}]])
             catch
                 _:TheError ->
                     lager:alert("CRASH ~p~n~p", [TheError, erlang:get_stacktrace()]),
                     mochijson2:encode([[{error,request_failed}]])
             end,
    case SReply of
        {ok, Reply} ->
            Json = mochijson2:encode(Reply),
            case Reply of
                [First|_] ->
                    Json2 = mochijson2:encode(First),
                    lager:info("REPLY [~s, ...]", [erlang:iolist_to_binary(Json2)]);
                _ ->
                    lager:info("REPLY [~s, ...]",[Json])
            end,
            HBody = io_lib:format("~s~n", [erlang:iolist_to_binary(Json)]),
            {HBody, wrq:set_resp_header("Content-type", "application/json", wrq:append_to_response_body(Json, ReqData)), Context};
        {nok, Error} ->
            lager:info("REPLY 400 error ~p",[Error]),
            {{halt, 400}, ReqData, Context}
    end.

-spec to_html(wrq:reqdata(), term()) -> {iodata(), wrq:reqdata(), term()}.
to_html(ReqData, Context) ->
    try to_html_int(ReqData, Context)
    catch
        _:Errors ->
            lager:alert("CRASH when  to_html_int ~p~n~p", [Errors, erlang:get_stacktrace()
                                                                     ]),
            {{halt, 400}, ReqData, Context}
    end.

to_html_int(ReqData, Context) ->
    Method = ReqData#wm_reqdata.method,
    Path = wrq:path_tokens(ReqData),
    {ok, Json} =
        case {Method, Path} of
            {'GET', ["users", Id]} ->
                User = users:get(list_to_binary(Id)),
                lager:info("got ~p", [User]),
                to_json(User);
            _ -> {ok,<<"">>}
        end,
    lager:info("WHAAT ~p", [Json]),
    HBody = io_lib:format("~s~n", [Json]),
    {HBody, ReqData, Context}.

-spec content_types_provided(wrq:reqdata(), term()) -> {[{iodata(), atom()}], wrq:reqdata(), term()}.
content_types_provided(ReqData, Context) ->
    {[{"text/html", to_html}], ReqData, Context}.

-spec content_types_accepted(wrq:reqdata(), term()) -> {[{iodata(), atom()}], wrq:reqdata(), term()}.
content_types_accepted(RD, Ctx) ->
    {[{"application/json", from_json}, {"text/html", to_html}], RD, Ctx}.

-spec resource_exists(wrq:reqdata(), term()) -> {boolean(), wrq:reqdata(), term()}.
resource_exists(ReqData, Context) ->
    Method = ReqData#wm_reqdata.method,
    Path = [P|_]   = wrq:path_tokens(ReqData),
    case lists:member({ Method ,P}, supported_resources()) of
        true ->
            {true, ReqData, Context};
        false ->
            lager:alert("request denied method ~p path ~p", [Method, Path]),
            {false, ReqData, Context}
    end.

-spec supported_resources() -> [{atom(), [iodata()]}].
supported_resources() ->
    [
     {'GET', "shopping_list"},
     {'PUT', "users"},
     {'POST', "users"},
     {'GET', "users"}
    ].

malformed_request(OReqData, Context) ->
    try malformed_request_int(OReqData, Context)
    catch
        _:Errors ->
            lager:alert("CRASH when  malformed_request_int ~p~n~p", [Errors, erlang:get_stacktrace()
                                                                     ]),
            {{halt, 400}, OReqData, Context}
    end.

malformed_request_int(OReqData, Context) ->
    lager:info("~norg context ~p~n", [Context]),
    %% we need to be authorized a bit earlier than what is usual..
    {Authorized, ReqData, NewCtx} = is_authorized(OReqData, Context),
    Path = wrq:path_tokens(ReqData),
%%    {{halt, 401}, ReqData, Context}.
    lager:info("~nWHAADDAD ~p~n~n", [ common:test(wrq:req_body(ReqData))]),
    case ReqData#wm_reqdata.method of
        Method when Method == 'POST' orelse
                    Method == 'PUT' ->
            case request_data(Method, Path, wrq:req_body(ReqData)) of
                [] ->
                    {true, ReqData, Context};
                Json ->
                    {false, ReqData, [{json, Json}| NewCtx]}
            end;
        _ ->
            {false, ReqData, Context}
    end.


is_authorized(ReqData, Context) ->
    UserAgent = string:tokens(wrq:get_req_header("user-agent", ReqData), "/ "),
    ExpectedProtocol = wrq:get_req_header("protocolversion", ReqData),
    %%shopit/1.4 CFNetwork/709.1 Darwin/13.3.0"
    %% ["shopit","1.61","(iPhone;","iOS","8.2;","Scale","2.00)"]
    {_,{Os,Version}} = lists:foldl(fun("shopit",{false,Vars}) ->
                                           {true,Vars};
                                      (V,{true,{O,_}}) ->
                                           {false,{O,V}};
                                      ("Darwin", {B,{_,V}})->
                                           {B,{ios,V}};
                                      ("(iPhone;", {B,{_,V}}) ->
                                           {B, {ios, V}};
                                      (_, {false,Vars}) ->
                                           {false,Vars}
                                   end,
                                   {false,{android,"0.0"}}, UserAgent), %% Find out ios version etc for now
    lager:info("Got OS ~s and version ~s", [Os,Version]),
    UD = case user_from_auth(wrq:get_req_header("authorization", ReqData)) of
             {UserType, UserId, Token} ->
                 BinUId = list_to_binary(UserId),
                 BinToken = list_to_binary(Token),
                 lager:debug("Got user ~p with Token ~p"),
                 ok;
             _ ->
                 malformed_error
         end,
    {true, ReqData, Context}.
    %% {false, ReqData, [{userdata, no_data_found}|
    %%                   Context]}.

user_from_auth("Basic" ++ Base64) ->
    Str = base64:mime_decode_to_string(Base64),
    case string:tokens(Str, ":") of
        [UserType, UserId, Token] ->
            {UserType, UserId, Token};
        Any ->
            lager:error("trying to decode authorization field, got ~p", [Any]),
            {error, wrong_format}
    end;
user_from_auth(_) ->
    {error, wrong_format}.

destructify(List) when is_list(List)->
    lists:map(fun destructify/1, List);
destructify({?JSONSTRUCT, Val}) ->
    destructify(Val);
destructify({Key, PossibleList}) ->
    {Key, destructify(PossibleList)};
destructify(Other) ->
    Other.

request_data(Method, Path, Data) ->
    try T = lists:flatten(destructify(mochijson2:decode(Data))),
          lager:info("Got data ~p~n~n", [T]),
          validate_req(Method,Path,T)
    catch
        _:Errors ->
            lager:alert("CRASH when decoding json ~p~n~p~nData: ~p", [Errors, erlang:get_stacktrace()
                                                                     , Data]),
            []
    end.

validate_req(_Method, _Path, Data) ->
    lager:info("returning data ~p~n",[Data]),
    Data.

process_post(ReqData, Context) ->
    try process_post_int(ReqData,Context)
    catch
        _:Errors ->
            lager:alert("CRASH when process post ~p~n~p", [Errors, erlang:get_stacktrace()
                                                                     ]),
            {{halt, 400}, ReqData, Context}
    end.

process_post_int(ReqData, Context) ->
    Method = ReqData#wm_reqdata.method,
    Any = wrq:req_body(ReqData),
    Url = wrq:path_tokens(ReqData),
    lager:info("context ~p", [Context]),
    [{json, Decoded}]  = proplists:lookup_all(json, Context),
    lager:info("~p ~p ~p", [Method, Url, Any]),
    Reply = try
                %% {ok, Result} = mgsv_server:send_message({Method, UD,
                %%                                          Url, Decoded}),
                ok
            catch
                _:Error ->
                    lager:alert("CRASH ~p~n~p~n", [Error, erlang:get_stacktrace()]),
                    mochijson2:encode([[{error,request_failed}]])
            end,
    Json = mochijson2:encode(Reply),
    HBody = io_lib:format("~s~n", [erlang:iolist_to_binary(Json)]),
    case Reply of
        [First|_] ->
            Json2 = mochijson2:encode(First),
            lager:info("REPLY [~s, ...]", [erlang:iolist_to_binary(Json2)]);
        _ ->
            lager:info("REPLY [~s, ...]",[HBody])
    end,
    {true, wrq:set_resp_header("Content-type", "application/json", wrq:append_to_response_body(HBody, ReqData)), Context}.
