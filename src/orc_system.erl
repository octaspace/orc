-module(orc_system).

-export([methods/0]).
-export([handle_request/4]).

-export([cpu_model_name/0]).

methods() ->
    [<<"GET">>, <<"POST">>].

handle_request(<<"GET">>, info, _Body, Req) ->
    CPUUsage = cpu_usage(),
    Response = #{
        erts             => orc:env(erts_version),
        uptime           => uptime(),
        version          => orc:version(),
        os_version       => orc:env(os_version),
        linux_distro     => orc:env(os_linux_distro),
        linux_release    => orc:env(os_linux_release),
        kernel_version   => orc:env(kernel_version),
        is_hive_os       => orc:env(is_hive_os),
        is_wsl           => orc:env(is_wsl),
        arch             => orc:env(system_arch),
        cpu              => CPUUsage,
        cpu_load_percent => cpu_load_percent(CPUUsage),
        cpu_model_name   => orc:env(cpu_model_name),
        gpu              => gpu_info(),
        cuda_version     => orc:env(cuda_version),
        memory           => memory_usage(),
        disk             => disk_usage()
    },
    {200, Response, Req};

handle_request(<<"POST">>, vrf_start, Body, Req) ->
    {200, orc_vrf:start(Body), Req};

handle_request(<<"POST">>, shell, #{<<"command">> := Command} = _Body, Req) ->
    {Status, Output} = orc_shell:exec(Command),
    Reply = #{
        status => Status,
        output => Output
    },
    {200, Reply, Req};

handle_request(<<"POST">>, settings, Body, Req) ->
    lists:foreach(
        fun(#{<<"name">> := Name, <<"value">> := Value}) ->
            persistent_term:put({config, Name}, Value)
        end,
        Body
    ),
    {200, undefined, Req};

handle_request(<<"POST">>, upgrade, Body, Req) ->
    #{
        <<"version">> := Version,
        <<"release">> := Release,
        <<"md5sum">>  := MD5Sum
    } = Body,

    case Version =:= orc:version() of
        true ->
            {200, #{message => <<"already installed">>}, Req};
        false ->
            case orc_system_upgrade:start(binary_to_list(Release), MD5Sum) of
                ok ->
                    timer:apply_after(5000, erlang, halt, [1]),
                    {200, #{message => <<"release downloaded">>}, Req};
                {error, Reason} ->
                    {400, #{message => Reason}, Req}
            end
    end;

handle_request(<<"GET">>, logs, undefined, Req) ->
    FileName = filename:join([orc:env(cwd), "orc-logs.tar.gz"]),
    {0, _} = orc_shell:exec(iolist_to_binary([
        "tar zcf ",
        FileName,
        " ",
        filename:join([orc:env(cwd), "log"])
    ])),
    {200, {sendfile, 0, filelib:file_size(FileName), FileName}, Req}.

uptime() ->
    element(1, erlang:statistics(wall_clock)) div 1000.

cpu_usage() ->
    Analyze =
        fun({N, UsageBy, Summary, _}, Acc) ->
            #{
                user   := User, 
                kernel := Kernel
            } = maps:from_list(UsageBy),
            #{idle := Idle} = maps:from_list(Summary),
            [#{
                core   => N + 1,
                kernel => Kernel,
                user   => User,
                idle   => Idle
            } | Acc]
        end,
    lists:foldl(Analyze, [], orc_sysmon:cpu_usage()).

memory_usage() ->
    maps:from_list(memsup:get_system_memory_data()).

disk_usage() ->
    DockerFS = orc:env(docker_filesystem),
    DockerDriver = orc:env(docker_driver),
    DockerRootDir = binary_to_list(orc:env(docker_root_dir)),
    case lists:keyfind(DockerRootDir, 1, disksup:get_disk_data()) of
        {DockerRootDir, KBytes, Capacity} when DockerFS =:= <<"xfs">>, DockerDriver =:= <<"overlay2">> ->
            Size = KBytes * 1024,
            Used = Size div 100 * Capacity,
            #{
                size         => Size,
                used         => Used,
                free         => Size - Used,
                used_percent => Capacity
            };
        _ -> #{size => 0, used => 0, free => 0, used_percent => 0}
    end.

cpu_model_name() ->
    {ok, CPUInfo} = file:read_file("/proc/cpuinfo"),
    cpu_model_name(binary:split(CPUInfo, <<"\n">>, [global, trim_all])).

cpu_model_name([<<"model name\t: ", Model/binary>> | _Rest]) -> Model;
cpu_model_name([<<"Model\t\t: ", Model/binary>> | _Rest]) -> Model;
cpu_model_name([_Param | Rest]) -> cpu_model_name(Rest);
cpu_model_name([]) -> undefined.

gpu_info() ->
    #{
        nvidia => orc_cache:get_or_set(gpu_info_nvidia, fun() -> orc_gpu:info(nvidia) end),
        amd    => orc_cache:get_or_set(gpu_info_amd, fun() -> orc_gpu:info(amd) end)
    }.

cpu_load_percent(Usage) ->
    Total =
        lists:foldl(
            fun(#{idle := Idle}, Acc) -> Acc + Idle end,
            0,
            Usage
        ),
    100 - (Total / length(Usage)).
