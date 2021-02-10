%% Copyright (c) 2021 - Jonathan De Wachter
%%
%% This source file is part of Rodeo which is released under the MIT license.
%% Please refer to the LICENSE file that can be found at the root of the
%% project directory.
%%
%% Written by Jonathan De Wachter <dewachter.jonathan@gmail.com>, January 2021

-ifdef(TEST).

make_foo_bar_specs() ->
  Action = #{
    name => "Bar",
    type => collection ,
    path => "bar",
    method => get,
    input => [],
    output => []
  },
  Entity = #{name => "Foo", path => "foo", actions => [Action]},

  #{entities => [Entity]}.

make_foo_specs(Actions) ->
  Entity = #{
    name => "Foo",
    description => "Awesome Foo for you!.",
    path => "foo",
    actions => Actions
  },

  #{entities => [Entity]}.

assert_body(Body, Code) ->
  #{
    <<"error">> := #{
      <<"code">>    := Code,
      <<"name">>    := _,
      <<"message">> := _
    }
  } = jiffy:decode(Body, [return_maps]),

  ok.

mock_foo_entity(Actions) ->
  meck:new(foo, [non_strict]),
  meck:expect(foo, init, fun() ->
    #{
      name => "Foo",
      description => "Awesome Foo for you!.",
      path => "foo",
      actions => Actions
    }
  end),

  ok.

do_server_check(Initialize, Terminate, Check, Port) ->
  application:ensure_all_started(cowboy),
  application:ensure_all_started(gun),

  %% Do the initialization (it should mock the entity modules and return the
  %% Rodeo specs).
  Specs = Initialize(),

  Routes = rodeo:build_routes(Specs, []),
  Dispatch = cowboy_router:compile([{'_', Routes}]),

	{ok, Server} = cowboy:start_clear(
		list_to_atom("my_http_listener" ++ integer_to_list(Port)),
		[{port, Port}],
		#{env => #{dispatch => Dispatch}}
	),

  {ok, Client} = gun:open("localhost", Port),

  %% Perform the actual testing.
  Check(Client),

  gun:close(Client),
  cowboy:stop_listener(Server),

  %% To uninitialize what was initialized at the beginning.
  Terminate(),

  ok.

-endif.