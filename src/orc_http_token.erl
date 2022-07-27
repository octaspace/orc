-module(orc_http_token).

-behaviour(cowboy_middleware).

-export([execute/2]).

-include_lib("kernel/include/logger.hrl").

execute(Req, Env) ->
    QS = cowboy_req:parse_qs(Req),
    #{token := Token} = orc_ident:fetch(),
    case lists:keyfind(<<"token">>, 1, QS) of
        {<<"token">>, Value} when Value =:= Token ->
            {ok, Req, Env};
        _ ->
            Reply = #{
                <<"message">> => <<"token is invalid or missing">>
            },
            ?LOG_ERROR("token is invalid or missing, qs: ~p", [QS]),
            NewReq = cowboy_req:set_resp_body(jsx:encode(Reply), Req),
            {stop, cowboy_req:reply(403, #{}, NewReq)}
    end.
