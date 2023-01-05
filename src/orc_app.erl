-module(orc_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    set_cwd(),
    init_logger(),
    orc_ident:store(),
    collect_docker_info(),
    set_common_config(),
    init_http_server(),
    orc_sup:start_link().

stop(_State) -> ok.

set_cwd() ->
    {ok, Cwd} = file:get_cwd(),
    persistent_term:put({config, cwd}, Cwd).

set_common_config() ->
    Arch =
        case erlang:system_info(system_architecture) of
            "x86_64-pc-linux-gnu"       -> x86_64;
            "aarch64-unknown-linux-gnu" -> aarch64;
            _                           -> unknown
    end,
    detect_linux_distro(),
    persistent_term:put({config, system_arch}, Arch),
    persistent_term:put({config, erts_version}, list_to_binary(erlang:system_info(version))),
    persistent_term:put({config, cpu_model_name}, orc_system:cpu_model_name()),
    persistent_term:put({config, is_hive_os}, filelib:is_regular("/etc/hiveos-release")).

init_logger() ->
    Config = #{
        level => debug,
        config => #{
            file               => "log/orc.log",
            max_no_files       => 10,
            max_no_bytes       => 10485760,
            sync_mode_qlen     => 1000,
            drop_mode_qlen     => 2000,
            flush_qlen         => 2000,
            burst_limit_enable => false
        },
        formatter => {logger_formatter, #{template => [time, " ", mfa, ":", line, " ", msg, "\n"], single_line => true}}
    },
    case os:getenv("ORC_DEBUG") of
        false ->
            logger:remove_handler(default);
        _ ->
            DefaultConfig = #{
                level => debug,
                config => #{
                    burst_limit_enable => false
                },
                formatter => {logger_formatter, #{template => [time, " ", msg, "\n"]}}
            },
            logger:set_handler_config(default, DefaultConfig)
    end,
    logger:set_primary_config(#{level => debug}),
    logger:add_handler(orc, logger_disk_log_h, Config).

init_http_server() ->
    PrivDir = code:priv_dir(orc),

    TransportOpts = [
        {port, http_listen_port()},
        {certfile, filename:join([PrivDir, "octa.crt"])},
        {keyfile, filename:join([PrivDir, "octa.key"])}
    ],

    Dispatch = cowboy_router:compile([
        {'_', [
            {"/system", orc_http_route, #{callback => orc_system, route => info}},
            {"/system/shell", orc_http_route, #{callback => orc_system, route => shell}},
            {"/system/settings", orc_http_route, #{callback => orc_system, route => settings}},
            {"/system/upgrade", orc_http_route, #{callback => orc_system, route => upgrade}},
            {"/system/logs", orc_http_route, #{callback => orc_system, route => logs}},
            {"/system/vrf/start", orc_http_route, #{callback => orc_system, route => vrf_start}},

            {"/docker/c", orc_http_route, #{callback => orc_docker, route => container_list}},
            {"/docker/c/:name", orc_http_route, #{callback => orc_docker, route => container}},
            {"/docker/c/:name/start", orc_http_route, #{callback => orc_docker, route => container_start}},
            {"/docker/c/:name/stop", orc_http_route, #{callback => orc_docker, route => container_stop}},
            {"/docker/c/:name/exec", orc_http_route, #{callback => orc_docker, route => container_exec}},
            {"/docker/c/:name/stat", orc_http_route, #{callback => orc_docker, route => container_stat}},
            {"/docker/c/:name/wait", orc_http_route, #{callback => orc_docker, route => container_wait}},
            {"/docker/c/:name/logs/:tail", orc_http_route, #{callback => orc_docker, route => container_logs}},
            {"/docker/c/:name/inspect", orc_http_route, #{callback => orc_docker, route => container_inspect}},
            {"/docker/c/:name/archive", orc_http_route, #{callback => orc_docker, route => container_archive}},

            {"/docker/v", orc_http_route, #{callback => orc_docker, route => volumes_list}},
            {"/docker/v/:name", orc_http_route, #{callback => orc_docker, route => volumes_create}}
        ]}
    ]),

    HTTPOpts = #{
        env                     => #{dispatch => Dispatch},
        middlewares             => [orc_http_token, cowboy_router, cowboy_handler],
        idle_timeout            => timer:seconds(7200),
        inactivity_timeout      => timer:seconds(1800),
        max_frame_size_received => 131072,           %% 127 KiB
        max_received_frame_rate => {10000, 2000},    %% No more 10000 frames during 2 seconds
        settings_timeout        => timer:seconds(10) %% Wait for SETTINGS ack
    },

    {ok, _} = cowboy:start_tls(https, TransportOpts, HTTPOpts).

http_listen_port() ->
    case os:getenv("ORC_PORT") of
        false -> 47015;
        Port -> list_to_integer(Port)
    end.

collect_docker_info() ->
    {ok, 200, Info} = docker:g(<<"/info">>),
    case maps:get(<<"DriverStatus">>, Info) of
        null -> ok;
        DriverStatus ->
            lists:foreach(
                fun
                    ([<<"Backing Filesystem">>, Value]) ->
                        persistent_term:put({config, docker_filesystem}, Value);
                    ([_Opt, _Value]) -> ok
                end,
                DriverStatus
            )
    end,
    persistent_term:put({config, docker_driver}, maps:get(<<"Driver">>, Info)),
    persistent_term:put({config, docker_root_dir}, maps:get(<<"DockerRootDir">>, Info)),
    persistent_term:put({config, kernel_version}, maps:get(<<"KernelVersion">>, Info)),
    persistent_term:put({config, os_version}, maps:get(<<"OperatingSystem">>, Info)).

detect_linux_distro() ->
    {0, Data} = orc_shell:exec("lsb_release -sci"),
    [Distro, Release] = binary:split(Data, <<"\n">>, [global, trim_all]),
    persistent_term:put({config, os_linux_distro}, string:lowercase(Distro)),
    persistent_term:put({config, os_linux_release}, Release).
