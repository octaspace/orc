-module(orc_system_upgrade).

-export([start/2]).

-include_lib("kernel/include/logger.hrl").

start(Release, MD5Sum) ->
    ?LOG_INFO("start system upgrade, download: ~s", [Release]),
    FileName = filename(),
    case download(Release, FileName) of
        ok ->
            stage_1(FileName, MD5Sum);
        _ -> {error, <<"release download is failed">>}
    end.

stage_1(FileName, MD5Sum) ->
    ?LOG_INFO("check md5sum, filename: ~s, md5sum: ~s", [FileName, MD5Sum]),
    case md5sum(FileName, MD5Sum) of
        ok -> ok;
        {error, Reason} = Error ->
            ?LOG_ERROR("check md5sum failed, reason: ~s", [Reason]),
            file:delete(FileName),
            Error
    end.

md5sum(FileName, MD5Sum) ->
    {0, Data} = orc_shell:exec("md5sum " ++ FileName),
    [Sum | _Rest] = binary:split(Data, <<$\s>>, [trim_all, global]),
    case Sum =:= MD5Sum of
        true -> ok;
        false ->
            Reason = iolist_to_binary([
                "md5sum mismatch, expected ",
                MD5Sum,
                " but got ",
                Sum
            ]),
            {error, Reason}
    end.

filename() ->
    filename:join([orc:env(cwd), "upgrade.tar.bz2"]).

download(URL, FileName) ->
    file:delete(FileName),
    case httpc:request(get, {URL, []}, [], [{sync, true}, {stream, FileName}]) of
        {ok, saved_to_file} -> ok;
        Error -> Error
    end.
