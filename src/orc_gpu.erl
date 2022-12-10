-module(orc_gpu).

-export([info/1]).
-export([parse_output/2]).

-include_lib("kernel/include/logger.hrl").

info(nvidia) -> gpu_info("cuda-gpu-info");
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
                _CC,
                _MultiCPUs,
                CudaCores,
                _CCThreads,
                GPUClock,
                MemClock,
                MemBandwidth,
                MemTotal,
                MemFree
            ] = binary:split(Info, <<",">>, [trim_all, global]),
            [#{
                model             => Model,
                cuda_cores        => orc:to_number(CudaCores),
                gpu_clock_mhz     => orc:to_number(GPUClock),
                mem_clock_mhz     => orc:to_number(MemClock),
                mem_total_mb      => orc:to_number(MemTotal),
                mem_free_mb       => orc:to_number(MemFree),
                mem_bandwidth_gbs => orc:to_number(MemBandwidth)
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
