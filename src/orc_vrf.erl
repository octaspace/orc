-module(orc_vrf).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([start/1]).
-export([stop/0]).

%% gen_server callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([terminate/2]).

-type rules() :: #{
    open_ports := [
        #{port := inet:port_number(), proto := tcp | udp}
    ]
}.

-export_type([rules/0]).

-record(state, {rules = {} :: rules()}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start(Rules) ->
    gen_server:call(?MODULE, {start, Rules}).

stop() ->
    gen_server:call(?MODULE, stop).

init([]) ->
    {ok, #state{}}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_call({start, Rules}, _From, State) ->
    {reply, ok, State#state{rules = Rules}};

handle_call(stop, _From, State) ->
    {reply, ok, State}.

terminate(_Reason, _State) -> ok.
