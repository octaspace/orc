-module(orc_shell).

-export([exec/1]).

-define(TIMEOUT, orc:env(<<"shell_timeout">>, 60) * 1000).

exec(Command) ->
    Port = open_port({spawn, Command}, [stream, in, stderr_to_stdout, eof, hide, exit_status, binary]),
    get_data(Port, <<>>, ?TIMEOUT).

get_data(Port, Acc, Timeout) ->
    receive
        {Port, {data, Bytes}} ->
            get_data(Port, <<Acc/binary, Bytes/binary>>, Timeout);
        {Port, eof} ->
            port_close(Port),
            ExitCode =
                receive
                    {Port, {exit_status, Code}} ->
                        Code
                end,
            {ExitCode, Acc}
    after Timeout ->
        {1, Acc}
    end.
