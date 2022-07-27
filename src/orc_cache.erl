-module(orc_cache).

-compile(no_auto_import).

-export([set/2]).
-export([set/3]).
-export([get/1]).
-export([get_or_set/2]).
-export([get_or_set/3]).

-define(DEFAULT_TTL, 86400).

set(Key, Value) ->
    set(Key, Value, ?DEFAULT_TTL).

set(Key, Value, TTL) ->
    persistent_term:put({?MODULE, Key}, {Value, erlang:system_time(second) + TTL}).

get(Key) ->
    Now = erlang:system_time(second),
    case persistent_term:get({?MODULE, Key}, undefined) of
        undefined -> undefined;
        {_Value, TTL} when TTL =< Now -> %% expired
            persistent_term:erase({?MODULE, Key}),
            undefined;
        {Value, _TTL} -> Value
    end.

get_or_set(Key, Fun) ->
    get_or_set(Key, Fun, ?DEFAULT_TTL).

get_or_set(Key, Fun, TTL) ->
    case get(Key) of
        undefined ->
            Result = Fun(),
            set(Key, Result, TTL),
            Result;
        Value -> Value
    end.
