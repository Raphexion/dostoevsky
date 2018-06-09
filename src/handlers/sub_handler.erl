-module(sub_handler).

-export([init/2,
	 content_types_provided/2]).

init(Req0, Opts) ->
    Method = cowboy_req:method(Req0),
    HasBody = cowboy_req:has_body(Req0),
    Req = maybe_sub(Method, HasBody, Req0),
    {ok, Req, Opts}.

content_types_provided(Req, State) ->
    {[
      {<<"application/json">>, sub_json}
     ], Req, State}.

maybe_sub(<<"POST">>, true, Req0) ->
    {ok, Body, Req} = read_body(Req0),
    try jiffy:decode(Body) of
	Data ->
	    lager:info("ok json encoded sub"),
	    response(Req, handle(Data))
    catch
	_:_ ->
	    lager:info("bad json encoded sub"),
	    failure_sub(Req)
    end;

maybe_sub(_, _, Req0) ->
    failure_sub(Req0).

%%%
%%
%%%

response(Req, ok) ->
    success_sub(Req);

response(Req, _) ->
    failure_sub(Req).

%%%
%%
%%%

failure_sub(Req) ->
    cowboy_req:reply(400, Req).

success_sub(Req) ->
    cowboy_req:reply(200, Req).

%%%
%%
%%%

handle({[]}) ->
    lager:info("empty body"),
    error;

handle({[{<<"topic">>, Topic}, {<<"url">>, Url}]}) ->
    db:attach(Url, Topic),
    ok;

handle(BadFormat) ->
    lager:warning("Bad formating ~p", [BadFormat]),
    error.

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
