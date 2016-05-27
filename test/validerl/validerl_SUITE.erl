-module(validerl_SUITE).
-author('jay@duomark.com').
-vsn('').

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([
         check_get_value_binary/1
        ]).

-include("validerl_common_test.hrl").

all() -> [
          check_get_value_binary
         ].

init_per_suite(Config) -> Config.
end_per_suite(Config)  -> Config.

-spec check_get_value_binary(config()) -> {comment, string()}.
check_get_value_binary(_Config) ->

    ct:comment("Check get_value for a binary"),
    Test_Binary = ?FORALL(Value, binary(),
                          case validerl:get_value(Value, binary) of
                              {ok, Bin} when is_binary(Bin) -> true;
                              _Other -> false
                          end),
    true = proper:quickcheck(Test_Binary),

    ct:comment("Check get_value for a string list"),
    Test_String = ?FORALL(Value, string(),
                          case validerl:get_value(Value, binary) of
                              {ok, Bin} when is_binary(Bin) -> true;
                              {error, {invalid_binary, Value}} -> true;
                              _Other -> ct:log("~p", [_Other]), false
                          end),
    true = proper:quickcheck(Test_String),
    {comment, ""}.
