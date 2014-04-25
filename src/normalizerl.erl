-module(normalizerl).
-author('hernanrivasacosta@gmail.com').

-export([normalize_proplist/2]).

-include("normalizerl.hrl").

%%==============================================================================
%% API
%%==============================================================================
-spec normalize_proplist(proplist_schema(), [{any(), any()}]) ->
  {ok, [{any(), any()}]} | {errors, {atom(), any()}}.
normalize_proplist(Schema, Proplist) ->
  GetValue = fun(ItemSchema) -> get_proplist_value(ItemSchema, Proplist) end,
  IsError  = fun({error, _}) -> true;
                (_Other)     -> false end,
  case lists:partition(IsError, lists:map(GetValue, Schema)) of
    {[], Values} -> {ok, Values};
    {Errors, _}  -> {errors, [Reason || {error, Reason} <- Errors]}
  end.

%%==============================================================================
%% Utils
%%==============================================================================
get_proplist_value({Key, Type, Required}, Proplist) ->
  get_proplist_value({Key, Type, Required, undefined}, Proplist);
get_proplist_value({Key, Type, Required, Constraints}, Proplist) ->
  case {get_value_from_proplist(Key, Proplist), Required} of
    {undefined, required} ->
      {error, {missing_property, Key}};
    {undefined, optional} ->
      undefined;
    {undefined, {default, Value}} ->
      Value;
    {{ok, {Key, Value}}, _} ->
      case {validerl:get_value(Value, Type), Constraints} of
        {{ok, Validated}, undefined} ->
          Validated;
        {{ok, Validated}, Constraints} ->
          case value_satisfies(Validated, Constraints) of
            true -> Validated;
            false -> {error, {value_mismatch, Value}}
          end;
        Error ->
          Error
      end
  end.

get_value_from_proplist(Key, Proplist) ->
  GetValue = fun(K) -> lists:keyfind(K, 1, Proplist) =/= false end,
  case {GetValue(Key), is_list(Key)} of
    {false, true} ->
      first_satisfying(GetValue, Key);
    {false, false} ->
      undefined;
    {Tuple, _} ->
      {ok, Tuple}
  end.

value_satisfies(Value, {matches, Value}) ->
  true;
value_satisfies(Value, {matches, Constraints}) ->
  value_satisfies(Value, Constraints);
value_satisfies(Value, [Value | _T]) ->
  true;
value_satisfies(Value, [_H | T]) ->
  value_satisfies(Value, T);
value_satisfies(_Value, _Any) ->
  false.

first_satisfying(_F, []) ->
  undefined;
first_satisfying(F, [H | T]) ->
  case V = F(H) of
    true -> {ok, {H, V}};
    _    -> first_satisfying(F, T)
  end.