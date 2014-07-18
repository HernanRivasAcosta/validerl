-module(validerl).
-author('hernanrivasacosta@gmail.com').

-export([get_value/2, is_of_type/2]).
-export([get_bool/1, get_binary/1, get_int/1, get_int/2, get_string/1,
         get_list/2, get_atom/1, get_tuple/2]).

-include("validerl.hrl").

%%==============================================================================
%% API
%%==============================================================================
-spec get_value(any(), validerl_type())     -> validerl_value() |
                                               invaliderl_value().
get_value(Value, boolean)                   -> get_bool(Value);
get_value(Value, binary)                    -> get_binary(Value);
get_value(Value, string)                    -> get_string(Value);
get_value(Value, integer)                   -> get_int(Value);
get_value(Value, positive_integer)          -> get_int(Value, {1, undefined});
get_value(Value, non_neg_integer)           -> get_int(Value, {0, undefined});
get_value(Value, negative_integer)          -> get_int(Value, {undefined, 0});
get_value(Value, non_pos_integer)           -> get_int(Value, {undefined, -1});
get_value(Value, {integer, Bounds}) ->
  {Lower, Upper} = Bounds,
  % Take advantage of the fact that integers are not validerl types to
  % distinguish an integer with bounds and a tuple
  case is_integer(Lower) orelse is_integer(Upper) of
    true -> get_int(Value, Bounds);
    false -> get_tuple(Value, {integer, Bounds})
  end;
get_value(Value, atom)                      -> get_atom(Value);
get_value(Value, Type) when is_tuple(Type)  -> get_tuple(Value, Type);
get_value(Value, [Type])                    -> get_list(Value, Type);
get_value(Value, any)                       -> {ok, Value};
get_value(Value, Type)                      -> validate_custom(Type, Value).

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
get_binary(Atom) when is_atom(Atom) ->
  {ok, atom_to_binary(Atom, utf8)};
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

-spec get_int(any(), bounds()) -> validerl_int() | invaliderl_int().
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

-spec get_list(any(), validerl_type()) -> {ok, any()} |
                                          invaliderl_value().
get_list(List, Type) ->
  get_list(List, Type, []).

get_list([], _Type, Acc) ->
  {ok, lists:reverse(Acc)};
get_list([H | T], Type, Acc) ->
  case get_value(H, Type) of
    {ok, Value} ->
      get_list(T, Type, [Value | Acc]);
    Error ->
      Error
  end;
get_list(NotAList, _Types, _Acc) ->
  {error, {not_a_list, NotAList}}.

-spec get_atom(any()) -> validerl_atom() | invaliderl_atom().
get_atom(Atom) when is_atom(Atom) ->
  {ok, Atom};
get_atom(Bin) when is_binary(Bin) ->
  {ok, binary_to_atom(Bin, utf8)};
get_atom(List) when is_list(List) ->
  case get_string(List) of
    {ok, Str}  -> {ok, list_to_atom(Str)};
    {error, _} -> {error, {invalid_atom, List}}
  end;
get_atom(Other) ->
  {error, {invalid_atom, Other}}.

-spec get_tuple(any(), tuple()) -> validerl_tuple() | invaliderl_tuple().
get_tuple(Tuple, Type) when is_tuple(Tuple) ->
  Types = tuple_to_list(Type),
  Values = tuple_to_list(Tuple),
  case get_values(Values, Types) of
    {ok, Data} -> {ok, list_to_tuple(Data)};
    Error -> Error
  end;
get_tuple(NotATuple, _Type) ->
  {error, {not_a_tuple, NotATuple}}.
  
%%==============================================================================
%% Utils
%%==============================================================================
-spec validate_custom(validerl_ext_call(), [any()]) -> {ok, any()} |
                                                     {error, any()}.                
validate_custom({Module, Function, Args}, Input) ->
  try
    {ok, Value} = erlang:apply(Module, Function, [Input | Args])
  catch
    Class:Exception -> handle_custom_function_error(Class, Exception)
  end;
validate_custom({Module, Function}, Input) ->
  validate_custom({Module, Function, []}, Input);
validate_custom(Module, Input) when is_atom(Module) ->
  validate_custom({Module, validate}, Input);
validate_custom(Fun, Input) ->
  try
    {ok, Value} = Fun(Input)
  catch
    Class:Exception -> handle_custom_function_error(Class, Exception)
  end.

handle_custom_function_error(error, undef) ->
  {error, undefined_function};
handle_custom_function_error(error, {Atom, Reason}) when is_atom(Atom) ->
  {error, {Atom, Reason}};
handle_custom_function_error(error, Reason) ->
  {error, {unexpected, Reason}};
handle_custom_function_error(Class, Exception) ->
  {error, {unexpected_error, {Class, Exception}}}.

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

get_values(Values, Types) ->
  get_values(Values, Types, []).

get_values([], [], Acc) ->
  {ok, lists:reverse(Acc)};
get_values([Value | Values], [Type | Types], Acc) ->
  case get_value(Value, Type) of
    {ok, ValidatedValue} ->
      get_values(Values, Types, [ValidatedValue | Acc]);
    Error ->
      Error
  end;
get_values(Values, _Types, Acc) ->
  {error, {bad_arity, length(Values) + length(Acc)}}.