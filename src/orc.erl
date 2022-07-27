-module(orc).

-export([version/0]).
-export([env/1]).
-export([env/2]).
-export([to_binary/1]).

version() ->
    Version = lists:keyfind(orc, 1, application:which_applications()),
    list_to_binary(element(3, Version)).

env(Option) ->
    env(Option, undefined).

env(Option, Default) ->
    persistent_term:get({config, Option}, Default).

to_binary(Value) when is_binary(Value) -> Value;
to_binary(Value) when is_integer(Value) -> integer_to_binary(Value).
