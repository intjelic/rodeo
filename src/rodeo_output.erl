%% Copyright (c) 2021 - Jonathan De Wachter
%%
%% This source file is part of Rodeo which is released under the MIT license.
%% Please refer to the LICENSE file that can be found at the root of the
%% project directory.
%%
%% Written by Jonathan De Wachter <dewachter.jonathan@gmail.com>, January 2021

%% @doc Functions to write the JSON body of the HTTP response.
%%
%% This module provides functions to process the output of the associated
%% Erlang function, check it against the output specifications, and convert it
%% into the JSON body of the HTTP response.
%%
%% @end

-module(rodeo_output).
-export([output_to_json/2]).

output_to_json(_Retvals, _Specs) -> ok.

% %% @doc Brief description.
% %%
% %% Long description.
% %%
% %% @end
% -spec output_to_json([{string(), any()}], rodeo:json_specs()) ->
%   {ok, binary()} | {error, [string()]}.
% output_to_json(Retvals, Specs) ->
%   case process_retvals(Specs, Retvals, [], []) of
%     {ok, DecodedJson} -> jiffy:encode(DecodedJson);
%     {error, Errors} -> {error, Errors}
%   end.

% process_retvals([{Name, Rules}|Specs], Retvals, DecodedJson, Errors) ->
%   case process_value(Name, Rules, Retvals) of
%     {ok, Value} -> process_retvals(Specs, Retvals, DecodedJson ++ [Value], Errors);
%     {error, Error} -> process_retvals(Specs, Retvals, DecodedJson, Errors ++ [Error])
%   end;
% process_retvals([], _Retvals, DecodedJson, []) ->
%   {ok, DecodedJson};
% process_retvals([], _Retvals, _DecodedJson, Errors) ->
%   {error, Errors}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

foo_test() -> ok.
bar_test() -> ok.

-endif.