-module(pub_handler).

-export([init/2,
	 content_types_provided/2]).

init(Req0, Opts) ->
    lager:info("pub"),
    Method = cowboy_req:method(Req0),
    HasBody = cowboy_req:has_body(Req0),
    Req = maybe_pub(Method, HasBody, Req0),
    {ok, Req, Opts}.

content_types_provided(Req, State) ->
    {[
      {<<"application/json">>, pub_json}
     ], Req, State}.

maybe_pub(<<"POST">>, true, Req0) ->
    {ok, Body, Req} = read_body(Req0),
    response(Req, handle(jiffy:decode(Body)));

maybe_pub(_, _, Req0) ->
    failure_pub(Req0).

response(Req, ok) ->
    success_pub(Req);

response(Req, _) ->
    failure_pub(Req).

failure_pub(Req) ->
    cowboy_req:reply(400, Req).

success_pub(Req) ->
    cowboy_req:reply(200, Req).

handle({[]}) ->
    lager:info("empty body"),
    error;

handle({[{Topic, Data}]}) ->
    lager:info("New Topic: ~p", [Topic]),
    ok;

handle(BadFormat) ->
    lager:warning("Bad formating ~p", [BadFormat]),
    error.

read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
	{ok, Data, Req} -> {ok, << Acc/binary, Data/binary >>, Req};
	{more, Data, Req} -> read_body(Req, << Acc/binary, Data/binary >>)
    end.

read_body(Req0) ->
    read_body(Req0, <<"">>).
