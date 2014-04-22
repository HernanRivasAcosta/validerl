-module(validerl).
-author('hernanrivasacosta@gmail.com').

-export([get_value/2]).
-export([get_bool/1, get_binary/1, get_int/1, get_int/2, get_string/1,
         get_int_list/1, get_int_list/2, get_list/2, is_of_type/2, get_atom/1]).

-include("validerl.hrl").

%%==============================================================================
%% API
%%==============================================================================
-spec get_value(any(), validerl_type()) -> {ok, any()} | validerl_error().
get_value(Value, boolean)            -> get_bool(Value);
get_value(Value, binary)             -> get_binary(Value);
get_value(Value, string)             -> get_string(Value);
get_value(Value, integer)            -> get_int(Value);
get_value(Value, {integer, Bounds})  -> get_int(Value, Bounds);
get_value(Value, int_list)           -> get_int_list(Value);
get_value(Value, {int_list, Bounds}) -> get_int_list(Value, Bounds);
get_value(Value, {list, Type})       -> get_list(Value, Type);
get_value(Value, atom)               -> get_atom(Value);
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

-spec get_binary(any()) -> validerl_binary() | invaliderl_binary().
get_binary(Bin) when is_binary(Bin) ->
  {ok, Bin};
get_binary(Other) ->
  case get_string(Other) of
    {ok, Value} -> {ok, list_to_binary(Value)};
    {error, _}  -> {error, {invalid_binary, Other}}
  end.

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
  F = fun(C) -> is_integer(C) andalso C >= 0 andalso C =< 255 end,
  case lists:all(F, Str) of
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

-spec get_list(any(), atom() | [atom()]) -> {ok, [any()]} | {error, any()}.
get_list(Term, Type) when is_atom(Type) ->
  get_list(Term, [Type]);
get_list(List, Types) when is_list(List) ->
  F = fun(E) -> lists:any(fun(T) -> is_of_type(E, T) end, Types) end,
  case lists:all(F, List) of
    true -> List;
    _    -> {error, bad_list_types}
  end;
get_list(Any, _Types) ->
  {error, {not_a_list, Any}}.

-spec get_atom(any()) -> validerl_atom() | invaliderl_atom().
get_atom(Atom) when is_atom(Atom) ->
  {ok, Atom};
get_atom(Bin) when is_binary(Bin) ->
  {ok, binary_to_atom(Bin, latin1)};
get_atom(List) when is_list(List) ->
  case get_string(List) of
    {ok, Str}  -> {ok, list_to_atom(Str)};
    {error, _} -> {error, {invalid_atom, List}}
  end;
get_atom(Other) ->
  {error, {invalid_atom, Other}}.

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

is_of_type(Element, _Type = atom) ->      is_atom(Element);
is_of_type(Element, _Type = binary) ->    is_binary(Element);
is_of_type(Element, _Type = bitstring) -> is_bitstring(Element);
is_of_type(Element, _Type = boolean) ->   is_boolean(Element);
is_of_type(Element, _Type = float) ->     is_float(Element);
is_of_type(Element, _Type = integer) ->   is_integer(Element);
is_of_type(Element, _Type = list) ->      is_list(Element);
is_of_type(Element, _Type = number) ->    is_number(Element);
is_of_type(Element, _Type = pid) ->       is_pid(Element);
is_of_type(Element, _Type = port) ->      is_port(Element);
is_of_type(Element, _Type = reference) -> is_reference(Element);
is_of_type(Element, _Type = tuple) ->     is_tuple(Element);
is_of_type(_Element, _Type = any) ->      true;
is_of_type(_Element, _Type) ->            false.