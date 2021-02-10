%% Copyright (c) 2021 - Jonathan De Wachter
%%
%% This source file is part of Rodeo which is released under the MIT license.
%% Please refer to the LICENSE file that can be found at the root of the
%% project directory.
%%
%% Written by Jonathan De Wachter <dewachter.jonathan@gmail.com>, January 2021

%% @private
-module(rodeo_handler).
-behavior(cowboy_handler).
-export([init/2]).

%% This function checks the path and method of the HTTP request and tries to
%% find a matching action specs. If it's matching against the specs of an
%% action with type `resource', the ID is added to the Cowboy request. Note
%% that if it's failing to match only because the HTTP method wasn't matching,
%% a hint is returned so the caller can return a more informative HTTP
%% response. It can also match with several specs but it should never happen
%% with a valid master specs.
%%
-spec find_specs(cowboy_req:req(), [rodeo:action_specs()]) ->
  no_specs |
  {no_specs, invalid_method} |
  {rodeo:action_specs(), cowboy_req:req()} |
  [rodeo:action_specs()].
find_specs(Request, Specs) ->
  %% Because we don't rely on Cowboy path matching mechanism  (see
  %% cowboy_req:bindings()), we must manually decompose the path ourselves and
  %% check individual parts against the specs (in other words, instead of
  %% having strict paths defined like this /api/foo/:id/bar, we have very global
  %% path defined like this /api/foo/[...], where 'foo' is the name of the
  %% entity).
  %%
  %% We can discard the first two tokens which are mandatory and common ('api'
  %% and 'foo' which is the entity name) and we can also know it's an invalid
  %% path if it has any more than 2 tokens. We're only interested in checking
  %% further if it's something like '/api/foo', '/api/foo/bar', '/api/foo/id'
  %% or '/api/foo/id/bar'. You notice that it never has more than 2 relevant
  %% tokens.
  %%

  %% The code is definitvely messy and weird, but it does the job for now (it's
  %% because it's hard to distinguish what is resource ID and what is an actual
  %% path segment, and also because the resource ID must be saved as it's used
  %% in later logic, for now the request headers is used to sotre this ID).
  Path = cowboy_req:path(Request),
  PathParts = case string:tokens(binary_to_list(Path), "/") of
    ["api", _]           -> [];
    ["api", _, Foo]      ->
      try list_to_integer(Foo) of
        FooInteger -> [FooInteger]
      catch
        error:badarg -> [Foo]
      end;
    ["api", _, Foo, Bar] ->
      try list_to_integer(Foo) of
        FooInteger -> [FooInteger, Bar]
      catch
        error:badarg -> [Foo, Bar]
      end;
    _ -> undefined
  end,

  %% Update request header very inegantly...
  Request0 = case PathParts of
    undefined -> Request;
    PathParts ->
      case lists:filter(fun(Part) -> is_number(Part) end, PathParts) of
        [Number] ->
          #{headers := Headers} = Request,
          NewHeaders = maps:put(rodeo_id, Number, Headers),
          maps:update(headers, NewHeaders, Request);
        [] -> Request
      end
  end,

  PathFilter = fun(ActionSpecs) ->
    #{
      path := Path0,
      type := Type
    } = ActionSpecs,

    case {Type, Path0, PathParts} of
      {collection, no_path, []}      -> true;
      {resource,   no_path, [MaybeId]} when is_number(MaybeId) -> true;
      {collection, Path0,   [Path0]} -> true;
      {resource,   Path0,   [MaybeId, Path0]} when is_number(MaybeId) -> true;
      _ -> false
    end
  end,

  MethodFilter = fun(ActionSpecs) ->
    #{method := Method0} = ActionSpecs,

    RawMethod = cowboy_req:method(Request),
    Method1 = case RawMethod of
      <<"GET">>    -> get;
      <<"POST">>   -> post;
      <<"PUT">>    -> put;
      <<"PATCH">>  -> patch;
      <<"DELETE">> -> delete;
      _            -> unsupported_method
    end,
    Method0 =:= Method1
  end,

  %% First step is to filter by path, then by methods; if the path didn't match
  %% any of the action specs, atom undefined is returned.
  case PathParts of
    undefined -> no_specs;
    _ ->
    case lists:filter(PathFilter, Specs) of
      [] -> no_specs;
      RemainingSpecs ->
        case lists:filter(MethodFilter, RemainingSpecs) of
          [] -> {no_specs, invalid_method};
          [FunctionSpecs] -> {FunctionSpecs, Request0};
          MultipleSpecs when is_list(MultipleSpecs) -> MultipleSpecs
        end
    end
  end.

-spec init(cowboy_req:req(), rodeo:entity_specs()) ->
  {ok, cowboy_req:req(), rodeo:entity_specs()}.
init(Request, Specs) ->
  Response0 = try
    %% Because the handler is associated to a given entity, it's given the list
    %% of action specs and we must compute which action specs this request is
    %% associated with.
    case find_specs(Request, Specs) of
      no_specs ->
        cowboy_req:reply(404, Request);
      {no_specs, invalid_method} ->
        cowboy_req:reply(405, Request);
      {ActionSpecs, Request0} ->
        process_request(Request0, ActionSpecs);
      ManySpecs when is_list(ManySpecs) ->
        ok
    end
  of
    Response -> Response
  catch
    X:Y:Stacktrace ->
      io:format("exception handled ~p and ~p~n", [X, Y]),
      erlang:display(Stacktrace),

      %% @todo Log what went wrong and add a json error message.
      cowboy_req:reply(500, Request)
  end,

  {ok, Response0, Specs}.

-spec process_request(cowboy_req:req(), rodeo_action:specs()) ->
  cowboy_req:req().
process_request(Request, Specs) ->
  % throw(foobar),

  %% Normalize the body; if there is no body, then we treat it as an emtpy JSON.
  {Json, Request1} = case cowboy_req:read_body(Request) of
    {ok, "", Request0}   -> {<<"{}">>, Request0};
    {ok, Data, Request0} -> {Data, Request0}
  end,
  io:format("json ~p~n", [Json]),

  InputSpecs = maps:get(input, Specs),
  case rodeo_input:json_to_input(Json, InputSpecs) of
    invalid_json ->
      rodeo_response:invalid_json(Request1);
    {ok, Parameters} ->
      execute_action(Request1, Specs, Parameters);
    {error, Errors} ->
      rodeo_response:invalid_input(Request1, Errors)
  end.

-spec execute_action(cowboy_req:req(), rodeo_action:specs(), [any()]) ->
  cowboy_req:req().
execute_action(Request, Specs, Parameters) ->
  %% Determine if there's an ID and add it to the list of parameters if there
  %% is one.
  Headers = maps:get(headers, Request, #{}),
  Parameters0 = case maps:get(rodeo_id, Headers, no_id) of
    no_id -> Parameters;
    Id -> [Id] ++ Parameters
  end,

  %% @todo When extra parameters is implemented in the specs, we will add them
  %%       to the list of parameters here.
  #{function := {Module, Function, Args}} = Specs,
  try
    erlang:apply(Module, Function, Parameters0 ++ Args)
  of
    ReturnValues -> process_response(Request, Specs, ReturnValues)
  catch
    %% @todo Define the valid and expected Rodeo exceptions the function can
    %%       throw and let the others exceptions be propagated to the caller.

    %% For now we can simply throw any exception, so the caller will catch it
    %% and return a 500 error response, which is what we want.
    _:_ -> throw(barfoo)
  end.

-spec process_response(cowboy_req:req(), rodeo_action:specs(), [any()]) ->
  cowboy_req:req().
process_response(Request, _Specs, _ReturnValues) ->
  rodeo_response:json_reply(Request, 200, {[]}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include("rodeo_fixtures.hrl").

find_specs_test() ->
  %% The following tests use "foo" as the entity name, but it could indeed be
  %% anything.
  Specs = [
    #{path => no_path, type => collection, method => get},
    #{path => "bar",   type => collection, method => get},
    #{path => no_path, type => resource,   method => get},
    #{path => "bar",   type => resource,   method => get},
    #{path => "bar",   type => resource,   method => post}
  ],

  Request0 = #{path => <<"/api/foo">>,        method => <<"GET">>,  headers => #{}},
  Request1 = #{path => <<"/api/foo/bar">>,    method => <<"GET">>,  headers => #{}},
  Request2 = #{path => <<"/api/foo/42">>,     method => <<"GET">>,  headers => #{}},
  Request3 = #{path => <<"/api/foo/42/bar">>, method => <<"GET">>,  headers => #{}},
  Request4 = #{path => <<"/api/foo/42/bar">>, method => <<"POST">>, headers => #{}},

  IsMatching = fun(Request, Index) ->
    MatchingSpecs = lists:nth(Index, Specs),
    {MatchingSpecs, ReturnedRequest} = find_specs(Request, Specs),

    %% If the action specs is of resource type, we must check further and see
    %% the returned request contains the resource ID.
    true = case maps:get(type, MatchingSpecs) of
      collection -> maps:get(headers, ReturnedRequest) =:= #{};
      resource -> maps:get(headers, ReturnedRequest) =:= #{rodeo_id => 42}
    end,

    ok
  end,

  ok = IsMatching(Request0, 1),
  ok = IsMatching(Request1, 2),
  ok = IsMatching(Request2, 3),
  ok = IsMatching(Request3, 4),
  ok = IsMatching(Request4, 5),

  %% Test with a request that doesn't match with any of the specs.
  no_specs = find_specs(#{
    path => <<"/api/foo/quz">>,
    method => <<"GET">>,
    headers => #{}
  }, Specs),

  %% Test with a request that doesn't match with any of the specs, but almost.
  {no_specs, invalid_method} = find_specs(#{
    path => <<"/api/foo/42/bar">>,
    method => <<"PATCH">>,
    headers => #{}
  }, Specs),

  %% Test with a request with an invalid path.
  no_specs = find_specs(#{
    path => <<"/api/foo/bar/quz">>,
    method => <<"GET">>,
    headers => #{}
  }, Specs),

  %% Test when it could match several specs (it will never happen if the master
  %% specs is valid).
  ActionSpecs1 = #{path => "bar", type => collection, method => get},
  ActionSpecs2 = #{path => "bar",   type => collection, method => get},
  [ActionSpecs1, ActionSpecs2] = find_specs(#{
    path => <<"/api/foo/bar">>,
    method => <<"GET">>,
    headers => #{}
  }, [ActionSpecs1, ActionSpecs2]),

  ok.

response_500_unhandled_exception_test() ->
  %% When any unexpected exception is raised during the process of a request it
  %% will be caught and should generate a 500 error response.
  % Initialize = fun() -> make_foo_bar_specs()  end,
  % Terminate = fun() -> do_nothing end,

  % Check = fun(Client) ->
  %   %% If any exception occurs during the process of the request, it will be
  %   %% caught and should generate a 505 error response.
  %   % meck:new(rodeo_handler, [unstick]),
  %   % meck:expect(rodeo_handler, process_request, fun(_, _) ->
  %   %   throw(foobar)
  %   % end),

  %   Stream = gun:get(Client, "/api/foo/bar"),
  %   case gun:await(Client, Stream) of
  %     {response, fin, Status, _} ->
  %       500 = Status
  %   end

  %   % meck:unload(rodeo_handler),
  % end,

  % do_server_check(Initialize, Terminate, Check),
  ok.

response_404_no_matching_specs_test() ->
  %% When the request doesn't match with anything in the specs, it's simply a
  %% 404 not found error response.
  Initialize = fun() -> make_foo_bar_specs() end,
  Terminate = fun() -> do_nothing end,

  Check = fun(Client) ->
    Stream = gun:get(Client, "/api/bar/foo"),
    case gun:await(Client, Stream) of
      {response, fin, Status, _} -> 404 = Status
    end
  end,

  do_server_check(Initialize, Terminate, Check, 8082),
  ok.

response_405_almost_matching_specs_test() ->
  %% When the request doesn't match with any of the action specs, but only
  %% because the method didn't match, it's not 404, it's 405.
  Initialize = fun() -> make_foo_bar_specs() end,
  Terminate = fun() -> do_nothing end,

  Check = fun(Client) ->
    Stream = gun:post(Client, "/api/foo/bar", []),
    case gun:await(Client, Stream) of
      {response, fin, Status, _} -> 405 = Status
    end
  end,

  do_server_check(Initialize, Terminate, Check, 8083),
  ok.

response_400_invalid_json_test() ->
  %% When it's failing to parse and read the JSON, it should returns a 400 bad
  %% request error response.
  % Initialize = fun() ->
  %   Actions = [
  %     #{
  %       path => "bar",
  %       type => collection,
  %       method => post
  %     }
  %   ],
  %   mock_foo_entity(Actions),

  %   #{entities => [foo]}
  % end,

  % Terminate = fun() -> meck:unload(foo) end,

  % Check = fun(Client) ->
  %   Path = "/api/foo/bar",
  %   Headers = [{<<"content-type">>, "application/json"}],
  %   Body = "{\"foo\": \"bar\"",

  %   Stream = gun:post(Client, Path, Headers, Body),
  %   case gun:await(Client, Stream) of
  %     {response, nofin, 400, _Headers} ->
  %       {ok, Body0} = gun:await_body(Client, Stream),
  %       ok = assert_body(Body0, <<"INVALID_JSON">>)
  %   end
  % end,

  % do_server_check(Initialize, Terminate, Check),
  ok.

response_400_invalid_input_test() ->
  %% When the content of the JSON is valid but its content doens't match with
  %% the input specs.
  % Initialize = fun() ->
  %   Actions = [
  %     #{
  %       path => "bar",
  %       type => collection,
  %       method => post,
  %       input => [
  %         #{
  %           name => "quz",
  %           type => number,
  %           required => true
  %         }
  %       ]
  %     }
  %   ],
  %   mock_foo_entity(Actions),

  %   #{entities => [foo]}
  % end,

  % Terminate = fun() -> meck:unload(foo) end,

  % Check = fun(Client) ->
  %   Path = "/api/foo/bar",
  %   Headers = [{<<"content-type">>, "application/json"}],
  %   Body = "{\"quz\": \"42\"}",

  %   Stream = gun:post(Client, Path, Headers, Body),
  %   case gun:await(Client, Stream) of
  %     {response, nofin, 400, _Headers} ->
  %       {ok, Body0} = gun:await_body(Client, Stream),
  %       ok = assert_body(Body0, <<"INVALID_INPT">>)
  %   end
  % end,

  % do_server_check(Initialize, Terminate, Check),
  ok.

response_500_execution_failure_test() ->
  %% When something went wrong during the execution of the Erlang function.
  % Initialize = fun() -> ok end,
  % Terminate = fun() -> ok end,

  % Check = fun(_Client) ->
  %   do_something
  % end,

  % do_server_check(Initialize, Terminate, Check),
  ok.

response_custom_error_test() ->
  %% When something went wrong during the execution of the Erlang function but
  %% it's under control and it's purposely returning a custom error message
  %% (usually it's a bad request and the input could only be checked at the
  %% time of the execution of the Erlang function).
  % Initialize = fun() -> ok end,
  % Terminate = fun() -> ok end,

  % Check = fun(_Client) ->
  %   do_something
  % end,

  % do_server_check(Initialize, Terminate, Check),
  ok.

response_500_invalid_output_test() ->
  %% When the execution of the Erlang function went right but the output
  %% produced isn't matching the output specs; this is an error-side error and
  %% debugging info must not be included in the HTTP response.
  % Initialize = fun() -> ok end,
  % Terminate = fun() -> ok end,

  % Check = fun(_Client) ->
  %   do_something
  % end,

  % do_server_check(Initialize, Terminate, Check),
  ok.

response_200_execution_success_test() ->
  %% When everything went right.
  % Initialize = fun() -> ok end,
  % Terminate = fun() -> ok end,

  % Check = fun(_Client) ->
  %   do_something
  % end,

  % do_server_check(Initialize, Terminate, Check),
  ok.

-endif.