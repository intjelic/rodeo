%% Copyright (c) 2020-2021 - Jonathan De Wachter
%%
%% This source file is part of Rodeo which is released under the MIT license. 
%% Please refer to the LICENSE file that can be found at the root of the 
%% project directory.
%%
%% Written by Jonathan De Wachter <dewachter.jonathan@gmail.com>, January 2021

-module(rodeo).
-export([hello_world/0]).

hello_world() ->
  io:format("Hello world~n").