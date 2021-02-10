%% Copyright (c) 2021 - Jonathan De Wachter
%%
%% This source file is part of Rodeo which is released under the MIT license.
%% Please refer to the LICENSE file that can be found at the root of the
%% project directory.
%%
%% Written by Jonathan De Wachter <dewachter.jonathan@gmail.com>, January 2021

%% @doc Format of the Rodeo entity specs.
%%
%% This module defines the format of a Rodeo entity specifications. Each
%% entity defines the list of actions.
%%
%% <b>[todo]</b> Document the format of the Rodeo entity specs here.

%% @end
-module(rodeo_entity).
-export_type([specs/0]).

-type specs() :: #{
  name        := string(),
  description := string(),
  path        := string(),
  actions     := [rodeo_action:specs()]
}. %% Format of the entity specs.