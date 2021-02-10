%% Copyright (c) 2021 - Jonathan De Wachter
%%
%% This source file is part of Rodeo which is released under the MIT license.
%% Please refer to the LICENSE file that can be found at the root of the
%% project directory.
%%
%% Written by Jonathan De Wachter <dewachter.jonathan@gmail.com>, January 2021

%% @doc Format of the Rodeo action specs.
%%
%% This module defines the specifications of Rodeo actions. Each action
%% operates on either the collection or a single resource. It's also associated
%% to a given Erlang function and its input and output are described by a set
%% of rules.
%%
%% <b>[todo]</b> Document the format of the Rodeo action specs here.
%%
%% @end
-module(rodeo_action).
-export_type([http_method/0, specs/0]).

-type http_method() :: get | post | put | patch | delete. %% Supported HTTP methods.

-type specs() :: #{
  name := string(),
  description := string(),
  type := resource | collection,
  path := string() | no_path,
  method := http_method(),
  input := rodeo_json:json_specs(),
  input_adaptor := fun((any()) -> any()),
  output := rodeo_json:json_specs(),
  output_adaptor := fun((any()) -> any()),
  function := {Module :: atom(), Function :: atom(), Args :: [any()]}
}. %% Format of the action specs.