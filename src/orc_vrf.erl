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
    open_tcp_ports := [inet:port_number()],
    open_udp_ports := [inet:port_number()],
    timeout        := timeout()
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
    start_tcp_servers(Rules),
    start_udp_servers(Rules),
    {reply, ok, State#state{rules = Rules}}.

terminate(_Reason, _State) -> ok.

start_tcp_servers(#{<<"open_tcp_ports">> := Ports, <<"timeout">> := Timeout}) ->
    lists:foreach(fun(Port) -> tcp_server(Port, Timeout) end, Ports).

start_udp_servers(#{<<"open_udp_ports">> := Ports, <<"timeout">> := Timeout}) ->
    lists:foreach(fun(Port) -> udp_server(Port, Timeout) end, Ports).

tcp_server(Port, Timeout) ->
    case gen_tcp:listen(Port, [binary, {reuseaddr, true}]) of
        {ok, Socket} ->
            ?LOG_INFO("TCP port ~p is open", [Port]),
            spawn(fun() -> ping_pong_tcp(Socket, Port, Timeout) end);
        {error, Reason} ->
            ?LOG_ERROR("can't open TCP port: ~p, reason: ~p", [Port, Reason])
    end.

udp_server(Port, Timeout) ->
    spawn(fun() -> ping_pong_udp(Port, Timeout) end).

ping_pong_tcp(LSocket, Port, Timeout) ->
    case gen_tcp:accept(LSocket, Timeout) of
        {ok, Socket} ->
            receive
                {tcp, Socket, <<"ping">>} ->
                    ?LOG_INFO("receive ping, proto: tcp, port: ~p", [Port]),
                    gen_tcp:send(Socket, <<"pong">>);
                _ -> ok
            after Timeout ->
                ?LOG_ERROR("TCP ping timeout, port: ~p", [Port])
            end,
            gen_tcp:close(Socket),
            gen_tcp:close(LSocket);
        {error, Reason} ->
            ?LOG_ERROR("can't accept TCP connection, port: ~p, reason: ~p", [Port, Reason]),
            gen_tcp:close(LSocket)
    end.

ping_pong_udp(Port, Timeout) ->
    case gen_udp:open(Port, [binary]) of
        {ok, Socket} ->
            ?LOG_INFO("UDP port ~p is open", [Port]),
            receive
                {udp, Socket, Peer, PeerPort, <<"ping">>} ->
                    ?LOG_INFO("receive ping, proto: udp, port: ~p", [Port]),
                    gen_udp:send(Socket, Peer, PeerPort, <<"pong">>);
                _ -> ok
            after Timeout ->
                ?LOG_ERROR("UDP ping timeout, port: ~p", [Port])
            end,
            gen_udp:close(Socket);
        {error, Reason} ->
            ?LOG_ERROR("can't open UDP port: ~p, reason: ~p", [Port, Reason])
    end.
