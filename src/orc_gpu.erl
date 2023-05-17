-module(orc_gpu).

-export([info/1]).
-export([parse_output/2]).

-include_lib("kernel/include/logger.hrl").

info(nvidia) ->
    Args = "--query-gpu=name,driver_version,pstate,pcie.link.gen.max,pcie.link.gen.current,temperature.gpu,utilization.gpu,utilization.memory,memory.total,memory.free,display_mode,display_active,fan.speed,power.limit --format=csv,nounits,noheader",
    gpu_info(lookup_nvidia_smi() ++ " " ++ Args);

info(amd) -> gpu_info("clinfo --json").

gpu_info(FileName) ->
    ExecPath = filename:join([orc:env(cwd), FileName]),
    case orc_shell:exec(ExecPath) of
        {0, Data} ->
            try
                parse_output(FileName, Data)
            catch
                _:Reason:Stack ->
                    ?LOG_ERROR("can'g parse output: ~p, cmd: ~s, reason: ~p, stack: ~p", [
                        Data, ExecPath, Reason, Stack
                    ]),
                    []
            end;
        Error ->
            ?LOG_ERROR("can't get GPU info, exec_path: ~s, error: ~p", [ExecPath, Error]),
            []
    end.

parse_output("cuda-gpu-info", Data) ->
    lists:foldl(
        fun(Info, Acc) ->
            [
                Model,
                DriverVersion,
                PState,
                PCIELinkMax,
                PCIELinkCurrent,
                TempGPU,
                UtilizationGPU,
                UtilizationMem,
                MemTotal,
                MemFree,
                DisplayMode,
                DisplayActive,
                FanSpeed,
                PowerLimit
            ] = binary:split(Info, <<", ">>, [trim_all, global]),
            [#{
                model             => Model,
                driver_version    => DriverVersion,
                pstate            => PState,
                pcie_link_max     => orc:to_number(PCIELinkMax),
                pcie_link_current => orc:to_number(PCIELinkCurrent),
                gpu_temperature   => orc:to_number(TempGPU),
                gpu_utilization   => orc:to_number(UtilizationGPU),
                mem_utilization   => orc:to_number(UtilizationMem),
                mem_total_mb      => orc:to_number(MemTotal),
                mem_free_mb       => orc:to_number(MemFree),
                display_mode      => DisplayMode,
                display_active    => DisplayActive,
                fan_speed         => orc:to_number(FanSpeed),
                power_limit_watt  => orc:to_number(PowerLimit)
            } | Acc]
        end,
        [],
        binary:split(Data, <<"\n">>, [global, trim_all])
    );
parse_output("clinfo --json", Data) ->
    lists:flatten(lists:foldl(
        fun(#{<<"online">> := Online}, Acc) ->
            [process_amd_online_gpu(Online) | Acc];
           (_Info, Acc) -> Acc
        end,
        [],
        maps:get(<<"devices">>, jsx:decode(Data))
    )).

process_amd_online_gpu(GPUs) -> process_amd_online_gpu(GPUs, []).

process_amd_online_gpu([], Acc) -> Acc;
process_amd_online_gpu([#{<<"CL_DEVICE_VENDOR">> := <<"Advanced Micro Devices, Inc.">>} = Info | Rest], Acc) ->
    process_amd_online_gpu(Rest, [#{
        model        => maps:get(<<"CL_DEVICE_BOARD_NAME_AMD">>, Info),
        mem_total_mb => maps:get(<<"CL_DEVICE_GLOBAL_MEM_SIZE">>, Info) / 1024 / 1024 %% CL_DEVICE_GLOBAL_MEM_SIZE in bytes
    } | Acc]);
process_amd_online_gpu([#{<<"CL_DEVICE_VENDOR">> := _Vendor} | Rest], Acc) ->
    process_amd_online_gpu(Rest, Acc).

lookup_nvidia_smi() ->
    case orc:env(is_wsl) of
        true ->
            "/usr/lib/wsl/lib/nvidia-smi";
        false ->
            "nvidia-smi"
    end.
