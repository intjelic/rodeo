%% Copyright (c) 2021 - Jonathan De Wachter
%%
%% This source file is part of Rodeo which is released under the MIT license.
%% Please refer to the LICENSE file that can be found at the root of the
%% project directory.
%%
%% Written by Jonathan De Wachter <dewachter.jonathan@gmail.com>, January 2021

%% @doc Format of the input and output specs.
%%
%% This module defines the specifications of input and output for each action.
%%
%% <b>[todo]</b> Document the format of the input and output specs here.
%%
%% @end
-module(rodeo_json).
-export_type([value_specs/0, json_specs/0]).

-type value_specs() :: #{
  name := string(),
  description := string(),
  type => boolean | number | string | array | object,
  validator => fun((any()) -> any()),
  default := any(),
  adaptor := fun((any()) -> any())
}. %% Format of a single value specs.
-type json_specs() :: [string() | value_specs()]. %% Format of the function input/output specs.