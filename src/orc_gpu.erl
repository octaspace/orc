-module(orc_gpu).

-export([info/1]).

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
                _:Reason:_Stack ->
                    ?LOG_ERROR("can'g parse output, cmd: ~s, reason: ~p", [ExecPath, Reason]),
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
                cuda_cores        => binary_to_integer(CudaCores),
                gpu_clock_mhz     => binary_to_integer(GPUClock),
                mem_clock_mhz     => binary_to_integer(MemClock),
                mem_total_mb      => binary_to_integer(MemTotal),
                mem_free_mb       => binary_to_integer(MemFree),
                mem_bandwidth_gbs => binary_to_float(MemBandwidth)
            } | Acc]
        end,
        [],
        binary:split(Data, <<"\n">>, [global, trim_all])
    );
parse_output("clinfo --json", Data) ->
    lists:foldl(
        fun(#{<<"online">> := [#{<<"CL_DEVICE_VENDOR">> := <<"Advanced Micro Devices, Inc.">>} = Info]}, Acc) ->
            #{
                <<"CL_DEVICE_BOARD_NAME_AMD">>  := Model,
                <<"CL_DEVICE_GLOBAL_MEM_SIZE">> := MemTotal
            } = Info,
            [#{
                model        => Model,
                mem_total_mb => MemTotal / (1024 * 1024)
            } | Acc];
           (_Info, Acc) -> Acc
        end,
        [],
        maps:get(<<"devices">>, jsx:decode(Data))
    ).
