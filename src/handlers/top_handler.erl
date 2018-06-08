-module(top_handler).

-export([init/2,
	 content_types_provided/2,
	 foo/2]).

init(Req, Opts) ->
    lager:info("request"),
    {cowboy_rest, Req, Opts}.

content_types_provided(Req, State) ->
    {[
      {<<"application/json">>, foo}
     ], Req, State}.

foo(Req, State) ->
    Body = <<"{\"rest\": \"Hello World!\"}">>,
    {Body, Req, State}.
