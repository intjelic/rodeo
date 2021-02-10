%% Copyright (c) 2021 - Jonathan De Wachter
%%
%% This source file is part of Rodeo which is released under the MIT license.
%% Please refer to the LICENSE file that can be found at the root of the
%% project directory.
%%
%% Written by Jonathan De Wachter <dewachter.jonathan@gmail.com>, January 2021

%% @doc Functions to read the JSON body of the HTTP request.
%%
%% This module provides functions to process the JSON body of the HTTP
%% request, check it against the input specifications, and convert it into the
%% input of the associated Erlang function. Note that the caller may want to do
%% further processing to add the ID and additional custom parameters.
%%
%% @end

-module(rodeo_input).
-export([json_to_input/2]).

%% @doc Convert JSON to parameters according to a given specs.
%%
%% If no error is found during the convertion, the parameters are returned in
%% a list. If one or more errors are found, a list of explicit error messages
%% are returned. Note that an invalid JSON will produce a different error.
%%
%% @end
-spec json_to_input(binary(), rodeo:json_specs()) ->
  invalid_json | {ok, [any()]} | {error, [string()]}.
json_to_input(Json, Specs) ->
  try jiffy:decode(Json) of
    %% @todo Rework that
    {DecodedJson} when is_list(DecodedJson) -> 
      io:format("TesTT ~p~n", [DecodedJson]),
      NormalizedDecodedJson = lists:map(fun
        ({Key, Value}) when is_binary(Value) ->
          {binary_to_list(Key), binary_to_list(Value)};
        ({Key, Value}) ->
          {binary_to_list(Key), Value}
      end, DecodedJson),
      io:format("TesTT ~p~n", [NormalizedDecodedJson]),
      process_json_specs(NormalizedDecodedJson, Specs)
  catch
    _:_ -> invalid_json
  end.

%% Processing the JSON and checking it against the specs consists of iterating
%% over the specs and check if the value is found in the JSON and if it's
%% maching the constraints. This function is also running the adaptators.
-spec process_json_specs([{string(), any()}], rodeo:json_specs()) ->
  {ok, [any()]} | {error, [string()]}.
process_json_specs(Json, Specs) ->
  process_json_specs(Specs, Json, [], []).

process_json_specs([ValueSpecs|Specs], Json, Params, Errors) ->
  case process_value_specs(ValueSpecs, Json) of
    {ok, Value} -> 
      process_json_specs(Specs, Json, Params ++ [Value], Errors);
    {error, Error} ->
      process_json_specs(Specs, Json, Params, Errors ++ [Error])
  end;
process_json_specs([], _Json, Params, []) ->
  {ok, Params};
process_json_specs([], _Json, _Params, Errors) ->
  {error, Errors}.

process_value_specs(Specs, Json) ->
  Name = maps:get(name, Specs),
  Default = maps:get(default, Specs, undefined),
  case proplists:get_value(Name, Json, Default) of
    undefined ->
      {error, lists:flatten(io_lib:format("~s is required; the value is missing", [Name]))};
    Value ->
      case check_type(Value, Specs) of
        ok ->
          case run_validator(Value, Specs) of
            valid -> {ok, run_adaptor_if_any(Value, Specs)};
            {invalid, Why} -> {error, lists:flatten(io_lib:format(Why, [Name]))}
          end;
        {error, Error} -> {error, lists:flatten(io_lib:format(Error, [Name]))}
      end
  end.

check_type(Value, Specs) ->
  case maps:get(type, Specs, badkey) of
    %% By default the type is not constrained.
    badkey -> ok;
    Type ->
      case Type of
        boolean when not is_boolean(Value) -> {error, "~s has wrong type; must be a boolean"};
        number  when not is_number(Value)  -> {error, "~s has wrong type; must be a number"};
        string  when not is_list(Value)    -> {error, "~s has wrong type; must be a string"};
        _ -> ok
      end
  end.

run_validator(Value, Specs) ->
  case maps:get(validator, Specs, badkey) of
    %% If no validator is given, the value is valid (not constrained).
    badkey -> valid;
    Validator ->
      case Validator(Value) of
        valid -> valid;
        {invalid, Why} -> {invalid, "~s is invalid; " ++ Why}
      end
  end.

run_adaptor_if_any(Value, Specs) ->
  case maps:get(adaptor, Specs, badkey) of
    %% If no adaptor is given, the value is left untouched.
    badkey  -> Value;
    Adaptor -> Adaptor(Value)
  end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

json_to_input_test() ->
  {ok, []} = json_to_input(<<"{}">>, []),
  invalid_json = json_to_input(<<"{">>, []),
  % {ok, ["bar"]} = json_to_input(<<"{\"foo\": \"bar\"}">>, ["foo"]),

  ok.

process_json_specs_test() ->

  ValidJson = [{"bar", "Hello world!"}],
  InvalidJson = [{"foo", "Hello world!"}],

  Specs = [
    #{name => "foo", type => number, default => 42},
    #{name => "bar", type => string}
  ],

  {ok, [42, "Hello world!"]} = process_json_specs(ValidJson, Specs),

  Errors = [
    "foo has wrong type; must be a number",
    "bar is required; the value is missing"
  ],
  {error, Errors} = process_json_specs(InvalidJson, Specs),

  ok.

process_value_specs_test() ->

  %% Check required and optional with its default.
  {ok, "bar"} = process_value_specs(#{name => "foo"}, [{"foo", "bar"}]),
  {error, "foo is required; the value is missing"} = process_value_specs(#{name => "foo"}, []),
  {ok, "bar"} = process_value_specs(#{name => "foo", default => 42}, [{"foo", "bar"}]),
  {ok, 42} = process_value_specs(#{name => "foo", default => 42}, []),

  %% Check if the type is constraint.
  {ok, 42} = process_value_specs(#{name => "foo", type => number}, [{"foo", 42}]),
  {error, "foo has wrong type; must be a number"} = process_value_specs(#{name => "foo", type => number}, [{"foo", "bar"}]),
  {error, "foo has wrong type; must be a number"} = process_value_specs(#{name => "foo", type => number, default => "bar"}, []),

  %% Check if the validator is run.
  Validator = fun
    (42) -> valid;
    (_)  -> {invalid, "not 42"}
  end,
  {ok, 42} = process_value_specs(#{name => "foo", validator => Validator}, [{"foo", 42}]),
  {error, "foo is invalid; not 42"} = process_value_specs(#{name => "foo", validator => Validator}, [{"foo", "bar"}]),

  %% Check if the adaptor is run.
  Adaptor = fun(V) -> integer_to_list(V) end,
  {ok, "42"} = process_value_specs(#{name => "foo", adaptor => Adaptor}, [{"foo", 42}]),

  ok.

check_type_test() ->
  %% Check when type must be a boolean.
  ok = check_type(false, #{type => boolean}),
  ok = check_type(true,  #{type => boolean}),
  {error, "~s has wrong type; must be a boolean"} = check_type(42, #{type => boolean}),

  %% Check when type must be a number.
  ok = check_type(42,    #{type => number}),
  ok = check_type(42.42, #{type => number}),
  {error, "~s has wrong type; must be a number"} = check_type(true, #{type => number}),

  %% Check when type must be a string.
  ok = check_type("Hello world", #{type => string}),
  ok = check_type("42",          #{type => string}),
  {error, "~s has wrong type; must be a string"} = check_type(42, #{type => string}),

  %% Check the type isn't enforced.
  ok = check_type(null, #{}),
  ok = check_type(false, #{}),
  ok = check_type(true, #{}),
  ok = check_type(42, #{}),
  ok = check_type("Hello world!", #{}),

  ok.

run_validator_test() ->
  Validator = fun
    (42) -> valid;
    (N) when N > 42 -> {invalid, "too big"};
    (N) when N < 42 -> {invalid, "too small"}
  end,

  valid = run_validator(42, #{validator => Validator}),
  {invalid, "~s is invalid; too small"} = run_validator(41, #{validator => Validator}),
  {invalid, "~s is invalid; too big"} = run_validator(43, #{validator => Validator}),

  ok.

-endif.