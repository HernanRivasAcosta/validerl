-module(validerl).
-author('hernanrivasacosta@gmail.com').

-export([get_value/2]).
-export([get_bool/1, get_int/1, get_int/2, get_string/1, get_int_list/1,
         get_int_list/2]).

-include("validerl.hrl").

%%==============================================================================
%% API
%%==============================================================================
-spec get_value(any(), validerl_type()) -> {ok, any()} | validerl_error().
get_value(Value, boolean)            -> get_bool(Value);
get_value(Value, string)             -> get_string(Value);
get_value(Value, integer)            -> get_int(Value);
get_value(Value, {integer, Bounds})  -> get_int(Value, Bounds);
get_value(Value, int_list)           -> get_int_list(Value);
get_value(Value, {int_list, Bounds}) -> get_int_list(Value, Bounds);
get_value(Value, Type)               -> validate_custom(Type, Value).

-spec get_bool(any()) -> validerl_bool() | invaliderl_bool().
get_bool(<<"1">>)     -> {ok, true};
get_bool(<<"0">>)     -> {ok, false};
get_bool("1")         -> {ok, true};
get_bool("0")         -> {ok, false};
get_bool("true")      -> {ok, true};
get_bool("false")     -> {ok, false};
get_bool(<<"true">>)  -> {ok, true};
get_bool(<<"false">>) -> {ok, false};
get_bool(true)        -> {ok, true};
get_bool(false)       -> {ok, false};
get_bool(1)           -> {ok, true};
get_bool(0)           -> {ok, false};
get_bool(Bool)        -> {error, {invalid_boolean, Bool}}.

-spec get_int(any()) -> validerl_int() | invaliderl_int().
get_int(Int) when is_integer(Int) ->
  {ok, Int};
get_int(Bin) when is_binary(Bin) ->
  get_int(binary_to_list(Bin));
get_int(Str) when is_list(Str) ->
  try {ok, list_to_integer(Str)}
  catch _:_ -> {error, {invalid_integer, Str}}
  end;
get_int(Other) ->
  {error, {invalid_integer, Other}}.

-spec get_int(any(), bounds()) -> validerl_int() | invaliderl_int() |
                                  out_of_range().
get_int(Value, {LowerBound, UpperBound}) ->
  case get_int(Value) of
    {ok, Int} ->
      case (LowerBound =:= undefined orelse LowerBound =< Int) andalso
           (UpperBound =:= undefined orelse UpperBound >= Int) of
        true -> {ok, Int};
        _    -> {error, {out_of_valid_range, Value}}
      end;
    Error     -> Error
  end.

-spec get_string(any()) -> validerl_string() | invaliderl_string().
get_string(Bin) when is_binary(Bin) ->
  {ok, binary_to_list(Bin)};
get_string(Str) when is_list(Str) ->
  case lists:all(fun(C) -> is_integer(C) end, Str) of
    true -> {ok, Str};
    _    -> {error, {invalid_string, Str}}
  end;
get_string(Atom) when is_atom(Atom) ->
  {ok, atom_to_list(Atom)};
get_string(Other) ->
  {error, {invalid_string, Other}}.

% This function might fail with inputs like [52, 54, 55], that is understood as
% [4, 6, 7], this is the result of defaulting to think the input is a string
-spec get_int_list(any()) -> validerl_int_list() | invaliderl_int_list().
get_int_list(Any) ->
  get_int_list(Any, {undefined, undefined}).

-spec get_int_list(any(), bounds()) -> validerl_int_list() |
                                       invaliderl_int_list() |
                                       out_of_range().
get_int_list(Bin, Bounds) when is_binary(Bin) ->
  get_int_list(binary_to_list(Bin), Bounds);
get_int_list(List, Bounds) when is_list(List) ->
  ValidChar = fun(C) ->
                (C >= $0 andalso C =< $9) orelse C =:= $, orelse C =:= 32
              end,
  IsInteger = fun(C) ->
                is_integer(C)
              end,
  case {lists:all(ValidChar, List), lists:all(IsInteger, List)} of
    {true, _} -> 
      Tokens = case lists:any(fun(C) -> C =:= $, end, List) of
                true -> string:tokens([C || C <- List, C =/= 32], ",");
                _    -> string:tokens(List, " ")
              end,
      F = fun([Int | T], Acc, Fun) ->
                case get_int(Int, Bounds) of
                  {ok, Value} -> Fun(T, [Value | Acc], Fun);
                  Error       -> Error
                end;
             ([], Acc, _Fun) -> {ok, lists:reverse(Acc)}
          end,
      F(Tokens, [], F);
    {_, true} -> {ok, List};
    _         -> {error, {invalid_integer_list, List}}
  end;
get_int_list(Other, _) ->
  {error, {invalid_integer_list, Other}}.

%%==============================================================================
%% Utils
%%==============================================================================
-spec validate_custom(validerl_ext_call(), any()) -> {ok, any()} |
                                                     {error, any()}.                
validate_custom({Module, {Function, Args}}, Input) ->
  try
    {ok, Value} = erlang:apply(Module, Function, [Input | Args])
  catch
    error:undef ->
      {error, {unexpected_type, Module}};
    error:{Atom, Reason} when is_atom(Atom) ->
      {error, {Atom, Reason}};
    error:Reason ->
      {error, {unexpected, Reason}};
    Class:Exception ->
      {error, {unexpected_error, {Class, Exception}}}
  end;
validate_custom({Module, Function}, Input) ->
  validate_custom({Module, {Function, []}}, Input);
validate_custom(Module, Input) ->
  validate_custom({Module, validate}, Input).