-module(orc_shell).

-export([exec/1]).

exec(Command) ->
    Port = open_port({spawn, Command}, [stream, in, stderr_to_stdout, eof, hide, exit_status, binary]),
    get_data(Port, <<>>).

get_data(Port, Acc) ->
    receive
        {Port, {data, Bytes}} ->
            get_data(Port, <<Acc/binary, Bytes/binary>>);
        {Port, eof} ->
            port_close(Port),
            ExitCode =
                receive
                    {Port, {exit_status, Code}} ->
                        Code
                end,
            {ExitCode, Acc}
    end.
