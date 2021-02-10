%% Copyright (c) 2021 - Jonathan De Wachter
%%
%% This source file is part of Rodeo which is released under the MIT license.
%% Please refer to the LICENSE file that can be found at the root of the
%% project directory.
%%
%% Written by Jonathan De Wachter <dewachter.jonathan@gmail.com>, January 2021

%% @doc Format of a Rodeo API specs and the Cowboy routes generator.
%%
%% This module contains the single function you will need to use, which is the
%% one that creates the Cowboy routes from your Rodeo API specifications along
%% with some optional options. It also defines the format of the API
%% specifications and all the options to control the generation of routes.
%%
%% <b>[todo]</b> Document the format of the API specs here.
%%
%% @end
-module(rodeo).
-export_type([
  version/0, license/0,
  api_specs/0,
  api_option/0, api_options/0
]).
-export([build_routes/2]).

-type version() :: {integer(), integer(), integer()}. %% Specify the current version of the API.

-type license() :: #{
  name := string(),
  url  := string()
}. %% Specify the license of the API.

-type api_specs() :: #{
  title       := string(),
  description := string(),
  version     := version(),
  license     := license(),
  entities    := [module()]
}. %% The format of the API specifications.

-type api_option() :: {with_doc, string()}. %% The individual options to control the generation of routes.
-type api_options() :: [api_option()]. %% Define the options to control the generation of routes.

%% @doc Generate the Cowboy routes from the API specs.
%%
%% This function generates the Cowboy routes from the API specifications that
%% is to be passed to Cowboy's route compile function.
%%
%% ```
%% Routes = rodeo:build_routes(Specs),
%% Dispatch = cowboy_router:compile([{'_', Routes}]).
%% '''
%%
%% The API options allows to tweak how the routes are generated such as their
%% paths, etc. For now, the only supported option is `with_doc' which, combined
%% with a path, will generate an addional route to serve the specs of the HTTP
%% API; it's useful to generate an HTTP API documention front-end side.
%%
%% @end
-spec build_routes(api_specs(), api_options()) ->
  [{Path :: iodata(), Handler :: module(), any()}].
build_routes(Specs0, Options) ->
  %% In general we do little defensive programming but here we really want to
  %% avoid catching silly specs-related mistakes at runtimes when clients
  %% start sending requests.
  Specs = normalize_specs(Specs0),

  %% Build Cowboy routes (a list of path with their handler); to keep it simple,
  %% this is one route path per entity, and the handler will take care of
  %% dispatching based on the actions specs.
  Routes = lists:map(fun(EntitySpecs) ->
    #{
      path    := Path,
      actions := Actions
    } = EntitySpecs,

    {"/api/" ++ Path ++ "/[...]", rodeo_handler, Actions}
  end, maps:get(entities, Specs)),

  %% Add doc generator handler if requested.
  case proplists:get_value(with_doc, Options) of
    undefined -> Routes;
    Path ->
      DocRoute = {"/api/" ++ Path, rodeo_doc_handler, Specs},
      Routes ++ [DocRoute]
  end.

%% A valid specs can come in various form and to simplify the logic that
%% process the specs, we need to normalize it. For now it only consists of
%% treating the input and output of actions, and change "foo" into
%% #{name => "foo"}.
normalize_specs(Specs) ->
  NormalizedEntitiesSpecs = lists:map(fun(EntitySpecs) ->
    NormalizedActionsSpecs = lists:map(fun(ActionSpecs) ->
      Input = maps:get(input, ActionSpecs),
      NormalizedInput = lists:map(fun(Value) ->
        case Value of
          Value when is_list(Value) ->
            #{name => Value};
          Value -> Value
        end
      end, Input),
      ActionSpecs0 = maps:update(input, NormalizedInput, ActionSpecs),

      Output = maps:get(output, ActionSpecs0),
      NormalizedOutput = lists:map(fun(Value) ->
        case Value of
          Value when is_list(Value) ->
            #{name => Value};
          Value -> Value
        end
      end, Output),
      maps:update(output, NormalizedOutput, ActionSpecs0)
    end, maps:get(actions, EntitySpecs)),

    maps:update(actions, NormalizedActionsSpecs, EntitySpecs)
  end, maps:get(entities, Specs)),

  maps:update(entities, NormalizedEntitiesSpecs, Specs).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include("rodeo_fixtures.hrl").

build_routes_test() ->
  Routes = build_routes(make_foo_bar_specs(), []),
  [{"/api/foo/[...]", rodeo_handler, _Actions}] = Routes,

  _ = cowboy_router:compile([{'_', Routes}]),

  ok.

build_routes_with_doc_test() ->
  Routes = build_routes(make_foo_bar_specs(), [{with_doc, "docs"}]),
  {"/api/docs", rodeo_doc_handler, _Actions} = lists:nth(2, Routes),

  _ = cowboy_router:compile([{'_', Routes}]),

  ok.

-endif.