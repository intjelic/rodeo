%% Copyright (c) 2021 - Jonathan De Wachter
%%
%% This source file is part of Rodeo which is released under the MIT license.
%% Please refer to the LICENSE file that can be found at the root of the
%% project directory.
%%
%% Written by Jonathan De Wachter <dewachter.jonathan@gmail.com>, January 2021

%% @private
-module(rodeo_response).
-export([json_reply/3, error_reply/5]).
-export([invalid_json/1, invalid_input/2]).

json_reply(Request, Status, Json) ->
  EncodedJson = jiffy:encode(Json),
  Headers = #{<<"content-type">> => "application/json"},
  cowboy_req:reply(Status, Headers, EncodedJson, Request).

error_reply(Request, Status, Code, Name, Message) ->
  %% Message must be JSON convertible.
  Json = #{
    <<"error">> => #{
      <<"code">> => list_to_binary(Code),
      <<"name">> => list_to_binary(Name),
      <<"message">> => Message
    }
  },
  json_reply(Request, Status, Json).

invalid_json(Request) ->
  error_reply(
    Request,
    400,
    "INVALID_JSON",
    "Invalid JSON",
    "The JSON body is invalid."
  ).

invalid_input(Request, Errors) ->
  error_reply(
    Request,
    400,
    "INVALID_INPUT",
    "Parameters in the JSON were incorrect.",
    lists:map(fun(E) -> list_to_binary(E) end, Errors)
  ).