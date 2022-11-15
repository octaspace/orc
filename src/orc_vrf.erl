-module(orc_vrf).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([start/1]).

%% gen_server callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([terminate/2]).

-include_lib("kernel/include/logger.hrl").

-type rules() :: #{
    open_tcp_ports => [inet:port_number()],
    open_udp_ports => [inet:port_number()]
}.

-export_type([rules/0]).

-record(state, {rules :: undefined | rules()}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start(Rules) ->
    gen_server:call(?MODULE, {start, Rules}).

init([]) ->
    {ok, #state{}}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_call({start, Rules}, _From, State) ->
    ?LOG_INFO("verification start, rules: ~p", [Rules]),
    lists:foreach(fun tcp_server/1, maps:get(<<"open_tcp_ports">>, Rules, [])),
    lists:foreach(fun udp_server/1, maps:get(<<"open_udp_ports">>, Rules, [])),
    {reply, ok, State#state{rules = Rules}}.

terminate(_Reason, _State) -> ok.

tcp_server(Port) ->
    case gen_tcp:listen(Port, [binary, {reuseaddr, true}]) of
        {ok, Socket} ->
            ?LOG_INFO("TCP port ~p is open", [Port]),
            spawn(fun() -> ping_pong_tcp(Socket, Port) end);
        {error, Reason} ->
            ?LOG_ERROR("can't open TCP port: ~p, reason: ~p", [Port, Reason])
    end.

udp_server(Port) ->
    spawn(fun() -> ping_pong_udp(Port) end).

ping_pong_tcp(LSocket, Port) ->
    case gen_tcp:accept(LSocket, 60000) of
        {ok, Socket} ->
            receive
                {tcp, Socket, <<"ping">>} ->
                    ?LOG_INFO("receive ping, proto: tcp, port: ~p", [Port]),
                    gen_tcp:send(Socket, <<"pong">>);
                _ -> ok
            after 60000 ->
                ?LOG_ERROR("TCP ping timeout, port: ~p", [Port])
            end,
            gen_tcp:close(Socket),
            gen_tcp:close(LSocket);
        {error, Reason} ->
            ?LOG_ERROR("can't accept TCP connection, port: ~p, reason: ~p", [Port, Reason]),
            gen_tcp:close(LSocket)
    end.

ping_pong_udp(Port) ->
    case gen_udp:open(Port, [binary]) of
        {ok, Socket} ->
            ?LOG_INFO("UDP port ~p is open", [Port]),
            receive
                {udp, Socket, Peer, PeerPort, <<"ping">>} ->
                    ?LOG_INFO("receive ping, proto: udp, port: ~p", [Port]),
                    gen_udp:send(Socket, Peer, PeerPort, <<"pong">>);
                _ -> ok
            after 60000 ->
                ?LOG_ERROR("UDP ping timeout, port: ~p", [Port])
            end,
            gen_udp:close(Socket);
        {error, Reason} ->
            ?LOG_ERROR("can't open UDP port: ~p, reason: ~p", [Port, Reason])
    end.
