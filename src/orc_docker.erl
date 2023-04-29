-module(orc_docker).

-export([methods/0]).
-export([handle_request/4]).

-define(TIMEOUT, orc:env(<<"docker_timeout">>, 3600) * 1000).

methods() ->
    [<<"GET">>, <<"POST">>, <<"DELETE">>].

handle_request(<<"GET">>, volumes_list, _Body, Req) ->
    {ok, 200, Volumes} = docker:g(<<"/volumes">>, ?TIMEOUT),
    {200, maps:get(<<"Volumes">>, Volumes), Req};

handle_request(<<"POST">>, volumes_create, Body, Req) ->
    Name = cowboy_req:binding(name, Req),
    {ok, Code, Message} = docker:p(<<"/volumes/create">>, Body#{name => Name}, ?TIMEOUT),
    {Code, Message, Req};

handle_request(<<"POST">>, container_archive, #{<<"path">> := Path}, Req) ->
    Name = cowboy_req:binding(name, Req),
    case docker:g({<<"/containers/", Name/binary, "/archive">>, [{<<"path">>, Path}]}, ?TIMEOUT) of
        {ok, 200, Data} ->
            {200, base64:encode(zlib:gzip(Data)), Req};
        {ok, Code, Data} ->
            {Code, Data, Req}
    end;

handle_request(<<"GET">>, container_inspect, _Body, Req) ->
    Name = cowboy_req:binding(name, Req),
    {ok, Code, Message} = docker:g(<<"/containers/", Name/binary, "/json">>, ?TIMEOUT),
    {Code, Message, Req};

handle_request(<<"GET">>, container_logs, _Body, Req) ->
    Name = cowboy_req:binding(name, Req),
    Opts = [
        {<<"stdout">>, <<"true">>},
        {<<"stderr">>, <<"true">>},
        {<<"timestamps">>, <<"true">>},
        {<<"follow">>, <<"false">>},
        {<<"tail">>, cowboy_req:binding(tail, Req)}
    ],
    {ok, Code, Message} = docker:g({<<"/containers/", Name/binary, "/logs">>, Opts}, ?TIMEOUT),
    NewMessage =
        case is_map(Message) of
            true -> Message;
            false -> base64:encode(zlib:gzip(Message))
        end,
    {Code, NewMessage, Req};

handle_request(<<"GET">>, container_list, _Body, Req) ->
    {ok, Code, Message} = docker:g(<<"/containers/json">>, ?TIMEOUT),
    {Code, Message, Req};

%% create container
handle_request(<<"POST">>, container, #{<<"Image">> := Image} = Body, Req) ->
    Name = cowboy_req:binding(name, Req),
    image_pull(Image),
    {ok, Code, Message} = docker:p({<<"/containers/create">>, [{<<"name">>, Name}]}, Body, ?TIMEOUT),
    {Code, Message, Req};

handle_request(<<"DELETE">>, container, _Body, Req) ->
    Name = cowboy_req:binding(name, Req),
    case docker:d(<<"/containers/", Name/binary>>, ?TIMEOUT) of
        {ok, 204, <<>>} -> ok;
        {ok, _Code, Reason} ->
            {error, Reason}
    end;

handle_request(<<"GET">>, container_start, _Body, Req) ->
    Name = cowboy_req:binding(name, Req),
    {ok, Code, Message} = docker:p(<<"/containers/", Name/binary, "/start">>, #{}, ?TIMEOUT),
    {Code, Message, Req};

handle_request(<<"GET">>, container_stop, _Body, Req) ->
    Name = cowboy_req:binding(name, Req),
    {ok, Code, Message} = docker:p(<<"/containers/", Name/binary, "/stop">>, #{}, ?TIMEOUT),
    {Code, Message, Req};

handle_request(<<"GET">>, container_kill, _Body, Req) ->
    Name = cowboy_req:binding(name, Req),
    {ok, Code, Message} = docker:p(<<"/containers/", Name/binary, "/kill">>, #{}, ?TIMEOUT),
    {Code, Message, Req};

handle_request(<<"GET">>, container_stat, _Body, Req) ->
    Name = cowboy_req:binding(name, Req),
    {ok, Code, Message} = docker:g(
        {<<"/containers/", Name/binary, "/stats">>, [{<<"stream">>, <<"false">>}, {<<"one-shot">>, <<"true">>}]},
        ?TIMEOUT
    ),
    {Code, Message, Req};

handle_request(<<"GET">>, container_wait, _Body, Req) ->
    Name = cowboy_req:binding(name, Req),
    {ok, Code, Message} = docker:p(<<"/containers/", Name/binary, "/wait">>, #{}, ?TIMEOUT),
    {Code, Message, Req};

handle_request(<<"POST">>, container_exec, #{<<"command">> := Command} = Body, Req) ->
    Name = cowboy_req:binding(name, Req),
    Env = maps:fold(
        fun(K, V, Acc) ->
            [<<K/binary, "=", (orc:to_binary(V))/binary>> | Acc]
        end,
        [],
        maps:get(<<"env">>, Body, #{})
    ),
    TTY = maps:get(<<"tty">>, Body, true),
    Detach = maps:get(<<"detach">>, Body, false),
    Params = #{
        <<"Cmd">> =>
            case is_binary(Command) of
                true ->
                    binary:split(Command, <<$\s>>, [global]);
                false ->
                    Command
            end,
        <<"AttachStdin">>  => false,
        <<"AttachStdout">> => true,
        <<"AttachStderr">> => true,
        <<"Tty">>          => TTY,
        <<"Env">>          => Env
    },
    {ok, Code, Message} =
        case docker:p(<<"/containers/", Name/binary, "/exec">>, Params) of
            {ok, 201, #{<<"Id">> := Id}} ->
                docker:p(<<"/exec/", Id/binary, "/start">>, #{<<"Tty">> => TTY, <<"Detach">> => Detach}, ?TIMEOUT);
            Error ->
                Error
        end,
    {Code, Message, Req}.

image_pull(Image) ->
    {ok, 200, Images} = docker:g(<<"/images/json">>, ?TIMEOUT),
    case lists:any(fun(#{<<"RepoTags">> := RT}) -> RT =/= null andalso lists:member(Image, RT) end, Images) of
        true -> ok; %% Image already pulled
        false ->
            {ok, 200, _Message} = docker:p({<<"/images/create">>, [{<<"fromImage">>, Image}]}, #{}, ?TIMEOUT)
    end.
