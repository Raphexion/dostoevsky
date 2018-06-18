-module(pub_handler).

-export([init/2]).

init(Req0, Opts) ->
    lager:info("pub"),
    {ok, Bindings} = maps:find(bindings, Req0),
    {ok, Topic} = maps:find(topic, Bindings),
    Method = cowboy_req:method(Req0),
    HasBody = cowboy_req:has_body(Req0),
    Req = maybe_pub(Method, HasBody, Topic, Req0),
    {ok, Req, Opts}.

maybe_pub(<<"POST">>, true, Topic, Req0) ->
    {ok, Body, Req} = read_body(Req0),
    db:post(Topic, Body),
    cowboy_req:reply(200, Req);

maybe_pub(_, _, _, Req) ->
    cowboy_req:reply(400, Req).

%%%
%%
%%%

read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
	{ok, Data, Req} -> {ok, << Acc/binary, Data/binary >>, Req};
	{more, Data, Req} -> read_body(Req, << Acc/binary, Data/binary >>)
    end.

read_body(Req0) ->
    read_body(Req0, <<"">>).
