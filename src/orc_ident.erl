-module(orc_ident).

-export([fetch/0]).
-export([store/0]).
-export([send_register_request/1]).

-include_lib("kernel/include/logger.hrl").

fetch() ->
    {ok, Data} = file:read_file("orc.ident"),
    binary_to_term(Data).

store() ->
    case filelib:is_file("orc.ident") of
        true ->
            check_is_registered();
        false ->
            Token = crypto:hash(sha256, [
                integer_to_binary(erlang:system_time(millisecond)),
                crypto:strong_rand_bytes(256)
            ]),
            Data = #{
                token         => string:lowercase(binary:encode_hex(Token)),
                is_registered => false
            },
            send_register_request(Data)
    end.

write(Data) ->
    ok = file:write_file("orc.ident", term_to_binary(Data, [compressed]), [sync]).

check_is_registered() ->
    case fetch() of
        #{is_registered := true} -> ok;
        Data ->
            send_register_request(Data)
    end.

send_register_request(#{token := Token} = Data) ->
    case httpc:request(get, {register_url() ++ Token, register_headers()}, [], []) of
        {ok, {{"HTTP/1.1", 202, _OK}, _Headers, _Body}} ->
            write(Data#{is_registered => true});
        _Error ->
            timer:apply_after(60000, ?MODULE, send_register_request, [Data])
    end.

register_url() ->
    case os:getenv("ORC_DEBUG") of
        false ->
            "https://api.octa.space/v1/hello/";
        _ ->
            os:getenv("ORC_REGISTER_URL", "http://localhost:9991/hello/")
    end.

register_headers() ->
    case os:getenv("ORC_DOMAIN_NAME") of
        false -> [];
        [] -> [];
        Name ->
            case inet:getaddr(Name, inet) of
                {ok, _IP} ->
                    [{"X-ORC-DOMAIN-NAME", list_to_binary(Name)}];
                {error, Reason} ->
                    ?LOG_ERROR("can't resolve X-ORC-DOMAIN-NAME, name: ~s, reason: ~p", [Name, Reason]),
                    exit(Reason)
            end
    end.
