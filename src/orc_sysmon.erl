-module(orc_sysmon).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([cpu_usage/0]).

%% gen_server callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).

-include_lib("kernel/include/logger.hrl").

-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

cpu_usage() ->
    gen_server:call(?MODULE, cpu_usage).

init([]) ->
    {ok, #state{}}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_call(cpu_usage, _From, State) ->
    {reply, cpu_sup:util([detailed, per_cpu]), State}.
