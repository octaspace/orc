-module(orc_http_route).

-export([init/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([allowed_methods/2]).

-export([handle_request/2]).
-export([delete_resource/2]).
-export([delete_completed/2]).

-include_lib("kernel/include/logger.hrl").

init(Req, Opts) ->
    {cowboy_rest, Req, Opts#{method => cowboy_req:method(Req)}}.

content_types_provided(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, []}, handle_request}
    ], Req, State}.

content_types_accepted(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, '*'}, handle_request},
        {{<<"multipart">>, <<"form-data">>, '*'}, handle_request}
    ], Req, State}.

allowed_methods(Req, #{callback := Callback} = State) ->
    {Callback:methods(), Req, State}.

handle_request(Req, #{callback := Callback, method := Method, route := Route} = State) ->
    {Body, Req0} = read_body(Req),
    try
        {Status, Response, NewReq} = Callback:handle_request(Method, Route, decode_body(Body), Req0),
        access_log(Req, Body, Status),
        reply(Status, Response, NewReq, State)
    catch
        throw:{Code, Reason} ->
            access_log(Req, Body, Code),
            reply(Code, #{reason => Reason}, Req, State);
        _:Reason:StackTrace ->
            ?LOG_ERROR("can't process request, state: ~p, reason: ~p, stacktrace: ~p, req: ~p", [
                State, Reason, StackTrace, Req
            ]),
            access_log(Req, Body, 500),
            reply(500, #{reason => <<"Internal error">>}, Req, State)
    end.

delete_resource(Req, State) ->
    {true, Req, State}.

delete_completed(Req, #{callback := Callback, route := Route} = State) ->
    access_log(Req, undefined, 200),
    case Callback:handle_request(<<"DELETE">>, Route, <<>>, Req) of
        ok ->
            {true, Req, State};
        {ok, Response} ->
            Req1 = cowboy_req:set_resp_body(jsx:encode(Response), Req),
            Req2 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Req1),
            {true, Req2, State};
        {error, Reason} ->
            Req1 = cowboy_req:set_resp_body(jsx:encode(Reason), Req),
            Req2 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Req1),
            {false, Req2, State};
        _ ->
            {false, Req, State}
    end.

reply(Status, Response, Req, State) ->
    case Response of
        Value when Value =:= undefined; Value =:= <<>> ->
            {stop, cowboy_req:reply(Status, Req), State};
        {sendfile, _Offset, _Length, _Filename} = SendFile ->
            {stop, cowboy_req:reply(
                Status,
                #{<<"content-type">> => <<"application/octet-stream">>},
                SendFile,
                Req
            ), State};
        _ ->
            Req1 = cowboy_req:set_resp_body(jsx:encode(Response), Req),
            Req2 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Req1),
            Reply = cowboy_req:reply(Status, Req2),
            {stop, Reply, State}
    end.

read_body(Req) ->
    case cowboy_req:header(<<"content-type">>, Req) of
        <<"application/json">> ->
            {ok, MaybeJSON, NewReq} = cowboy_req:read_body(Req),
            {MaybeJSON, NewReq};
        _ -> {undefined, Req}
    end.

decode_body(Body) ->
    case jsx:is_json(Body) of
        true -> jsx:decode(Body);
        false -> undefined
    end.

access_log(Req, Body, Status) ->
    #{
        peer   := {ClientIP, _ClientPort},
        method := Method,
        path   := Path,
        qs     := QS
    } = Req,
    ?LOG_INFO("peer: ~s, method: ~s, path: ~s, status: ~p, body: ~s, qs: ~s", [
        inet:ntoa(ClientIP),
        Method,
        Path,
        Status,
        truncate_body(Body),
        QS
    ]).

truncate_body(Body) when is_binary(Body), byte_size(Body) >= 8192 ->
    <<(binary:part(Body, 0, 8192))/binary, "...">>;
truncate_body(Body) -> Body.
