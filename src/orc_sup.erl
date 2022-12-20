-module(orc_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Childs = [
        #{id => orc_vrf, start => {orc_vrf, start_link, []}}
        #{id => orc_sysmon, start => {orc_sysmon, start_link, []}}
    ],
    {ok, {{one_for_all, 0, 1}, Childs}}.
