%% Copyright (c) 2021 - Jonathan De Wachter
%%
%% This source file is part of Rodeo which is released under the MIT license.
%% Please refer to the LICENSE file that can be found at the root of the
%% project directory.
%%
%% Written by Jonathan De Wachter <dewachter.jonathan@gmail.com>, January 2021

%% @private
-module(rodeo_doc_handler).
-behavior(cowboy_handler).
-export([init/2]).

process_specs(Specs) ->
  %% This list of keys is to define the relevant data to be included in the
  %% response, and also their order.
  Keys = [title, description, version, license, entities],
  process_specs(Specs, Keys, []).

process_specs(Specs, [Key|Keys], Elements) ->
  Value = maps:get(Key, Specs, undefined),
  Element = case {Key, Value} of
    {title, Value} when Value =/= undefined ->
      {<<"title">>, list_to_binary(Value)};
    {description, Value} when Value =/= undefined ->
      {<<"description">>, list_to_binary(Value)};
    %% @todo Add missing processing of version and license of the API specs.
    {entities, EntitiesSpecs} ->
      EntityObjects = lists:map(fun(EntitySpecs) ->
        {process_entity_specs(EntitySpecs)}
      end, EntitiesSpecs),
      {<<"entities">>, EntityObjects};
    _ -> no_element
  end,

  case Element of
    no_element -> process_specs(Specs, Keys, Elements);
    Element -> process_specs(Specs, Keys, Elements ++ [Element])
  end;
process_specs(_Specs, [], Elements) ->
  Elements.

process_entity_specs(Specs) ->
  Keys = [name, description, path, actions],
  process_entity_specs(Specs, Keys, []).

process_entity_specs(Specs, [Key|Keys], Elements) ->
  Value = maps:get(Key, Specs, undefined),
  Element = case {Key, Value} of
    {name, Value} when Value =/= undefined ->
      {<<"name">>, list_to_binary(Value)};
    {description, Value} when Value =/= undefined ->
      {<<"description">>, list_to_binary(Value)};
    {path, Value} when Value =/= undefined ->
      {<<"path">>, list_to_binary(Value)};
    {actions, ActionsSpecs} ->
      ActionObjects = lists:map(fun(ActionSpecs) ->
        {process_action_specs(ActionSpecs)}
      end, ActionsSpecs),
      {<<"actions">>, ActionObjects};
    _ -> no_element
  end,

  case Element of
    no_element -> process_entity_specs(Specs, Keys, Elements);
    Element -> process_entity_specs(Specs, Keys, Elements ++ [Element])
  end;
process_entity_specs(_Specs, [], Elements) ->
  Elements.

process_action_specs(Specs) ->
  Keys = [name, description, type, path, method, input, output],
  process_action_specs(Specs, Keys, []).

process_action_specs(Specs, [Key|Keys], Elements) ->
  Value = maps:get(Key, Specs, undefined),
  Element = case {Key, Value} of
    {name, Value} when Value =/= undefined ->
      {<<"name">>, list_to_binary(Value)};
    {description, Value} when Value =/= undefined ->
      {<<"description">>, list_to_binary(Value)};
    {type, Value} ->
      {<<"type">>, list_to_binary(atom_to_list(Value))};
    {path, Value} ->
      case Value of
        no_path -> {<<"path">>, null};
        Path    -> {<<"path">>, list_to_binary(Path)}
      end;
    {method, Value} ->
      {<<"method">>, list_to_binary(atom_to_list(Value))};
    {input, Value} ->
      {<<"input">>, process_json_specs(Value)};
    {output, Value} ->
      {<<"output">>, process_json_specs(Value)};
    _ -> no_element
  end,

  case Element of
    no_element -> process_action_specs(Specs, Keys, Elements);
    Element -> process_action_specs(Specs, Keys, Elements ++ [Element])
  end;
process_action_specs(_Specs, [], Elements) ->
  Elements.

process_json_specs(Specs) ->
  lists:map(fun(S) -> {process_value_specs(S)} end, Specs).

process_value_specs(Specs) ->
  Keys = [name, description, type, default],
  process_value_specs(Specs, Keys, []).

process_value_specs(Specs, [Key|Keys], Elements) ->
  Value = maps:get(Key, Specs, undefined),
  Element = case {Key, Value} of
    {name, Value} when Value =/= undefined ->
      {<<"name">>, list_to_binary(Value)};
    {description, Value} when Value =/= undefined ->
      {<<"description">>, list_to_binary(Value)};
    {type, Value} when Value =/= undefined ->
      {<<"type">>, list_to_binary(atom_to_list(Value))};
    %% @todo Probably add more clauses here... In fact, the specs should be
    %% updated to provide a convertor.
    {default, Value} when Value =/= undefined ->
      {<<"default">>, Value};
    _ -> no_element
  end,

  case Element of
    no_element -> process_value_specs(Specs, Keys, Elements);
    Element -> process_value_specs(Specs, Keys, Elements ++ [Element])
  end;

process_value_specs(_Specs, [], Elements) ->
  Elements.

init(Request0, Specs) ->
  Body = jiffy:encode({process_specs(Specs)}),

  Request = cowboy_req:reply(200,
    #{<<"content-type">> => <<"application/json">>},
    Body,
    Request0
  ),
  {ok, Request, Specs}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include("rodeo_fixtures.hrl").

rodeo_doc_handler_test() ->
  application:ensure_all_started(cowboy),
  application:ensure_all_started(gun),

  Specs = #{
    title => "My HTTP API",
    description => "Awesome HTTP API for you!.",
    entities => [
      #{
        name => "Foo",
        description => "Awesome Foo for you!.",
        path => "foo",
        actions => [
          #{
            name => "Bar",
            description => "Awesome Bar for you!.",
            type => collection,
            path => no_path,
            method => get,
            input => [
              #{
                name => "aaa",
                description => "description of AAA",
                type => number,
                default => 42
              },
              "bbb"
            ],
            output => [
              #{
                name => "ccc",
                type => boolean
              }
            ]
          },
          #{
            name => "Quz",
            type => resource,
            path => "quz",
            method => post,
            input => [],
            output => []
          }
        ]
      }
    ]
  },
  Options = [{with_doc, "docs"}],

  Routes = rodeo:build_routes(Specs, Options),
  Dispatch = cowboy_router:compile([{'_', Routes}]),

	{ok, Server} = cowboy:start_clear(
		my_http_listener,
		[{port, 8080}],
		#{env => #{dispatch => Dispatch}}
	),

  {ok, Client} = gun:open("localhost", 8080),

  StreamRef = gun:get(Client, "/api/docs"),
  case gun:await(Client, StreamRef) of
    {response, nofin, Status, _} ->
      200 = Status,
      {ok, Body} = gun:await_body(Client, StreamRef),
      {[
        {<<"title">>, <<"My HTTP API">>},
        {<<"description">>, <<"Awesome HTTP API for you!.">>},
        {<<"entities">>, [
          {[
            {<<"name">>, <<"Foo">>},
            {<<"description">>, <<"Awesome Foo for you!.">>},
            {<<"path">>, <<"foo">>},
            {<<"actions">>, [
              {[
                {<<"name">>, <<"Bar">>},
                {<<"description">>, <<"Awesome Bar for you!.">>},
                {<<"type">>, <<"collection">>},
                {<<"path">>, null},
                {<<"method">>, <<"get">>},
                {<<"input">>, [
                  {[
                    {<<"name">>, <<"aaa">>},
                    {<<"description">>, <<"description of AAA">>},
                    {<<"type">>, <<"number">>},
                    {<<"default">>, 42}
                  ]},
                  {[
                    {<<"name">>, <<"bbb">>}
                  ]}
                ]},
                {<<"output">>, [
                  {[
                    {<<"name">>, <<"ccc">>},
                    {<<"type">>, <<"boolean">>}
                  ]}
                ]}
              ]},
              {[
                {<<"name">>, <<"Quz">>},
                {<<"type">>, <<"resource">>},
                {<<"path">>, <<"quz">>},
                {<<"method">>, <<"post">>},
                {<<"input">>, []},
                {<<"output">>, []}
              ]}
            ]}


          ]}
        ]}

      ]} = jiffy:decode(Body)
  end,

  gun:close(Client),
  cowboy:stop_listener(Server),

  ok.

-endif.