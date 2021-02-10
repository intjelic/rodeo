# Rodeo

> This is a work-in-progress; the Entity behavior was recently removed, making
part of the docs irrelevant.

From API specs to Cowboy handlers

Rodeo complements Cowboy with a generic handler and encourages you to implement
an HTTP API from a well-defined specifications. The result is less intrusive
as you are simply wrapping your existing regular Erlang functions and define
their input and output.

**Features**

- Convert entities and actions...
- ... to HTTP collections and resources
- Exclusively JSON-oriented
- Purposely very very very simple
- ... and thus discarding some standards.
- Built-in automatic API docs generator
- It's a work-in-progress; it's evolving as personal need arise.

Written by **Jonathan De Wachter** on January 2021 and licensed under the
**MIT license**.

## Preview

Instead of implementing Cowboy handlers, you implement the **Rodeo entity**
behavior. The `specs/0` function describes how the HTTP requests and responses
map to their 1:1 Erlang functions.

```
-module(hello)
-behavior(rodeo_entity).
-export([specs/0, hello/1]).

specs() ->
  #{
    path => "hello",
    actions => [
      #{
        type => collection,
        path => no_path,
        method => get,
        input => [#{name => "name", type => string}],
        output => [],
        mfa => {?MODULE, hello, []}
      }
    ]
  }.

hello(Name) ->
    io:format("Hello ~s!~n", [Name]).
```

Later, you're able to list all your entities in a master specifications in
order to generate the **Cowboy routes**. Under the hood, it's using a **generic
handler** that is provided by Rodeo.

```
Specs = #{
    title    => "My HTTP API",
    version  => {0, 1, 0},
    entities => [hello]
},
Routes = rodeo:generate_routes(Specs),
cowboy:compile(Routes).
```

Your Erlang functions are called according to the specifications you gave and
all the generic middleware logic is taken care for you. It includes
authentication, sanitizing the input, returning proper error messages. Also,
it's JSON-oriented.

```
GET /api/v1/hello
{
  "name": "world"
}
```
```
Hello world!
```

## Philosophy

Some philosophy was followed when making a number of design decisions, and they
might or might not suit your need.

While some strict standards emerged over the years (*OpenAPI* for instance),
Rodeo doesn't aim to fully comply with them. In fact, unless your API is used
by thousands of other developers, there's little benefit in following them
closely and in practice, it takes a lot of energy. Nonetheless, it's important
to meet some general expectation of an HTTP API and thus concepts like
resources and collections as well as the meaning of each HTTP methods are
implemented. They're well explained in the [Rodeo documentation](/doc/overview.edoc).

It's also exclusively JSON-oriented; while some folks will find some very
valid arguments against it, it does the job; it's sufficiently fast and most
importantly, it yields elegant code. The resource IDs are put in the URL, but
other than that, it's all exclusively in the JSON body.

Also Rodeo doesn't claim to be a perfect fit for all scenarios, but instead,
claims to be an efficient tool for most scenarios. Plus, it can easily be
combined by other tools. After all, it's merely providing a single generic
Cowboy handler and generate Cowboy routes for you; feel free to provide your
own very custom routes and handlers.

## Documentation

The documentation is all contained in `doc/overview.edoc` and an EDoc that
includes the API reference that can be generated with the `make edoc` command.