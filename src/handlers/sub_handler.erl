-module(sub_handler).

-export([init/2]).

init(Req0, Opts) ->
    Method = cowboy_req:method(Req0),
    HasBody = cowboy_req:has_body(Req0),
    Req = maybe_sub(Method, HasBody, Req0),
    {ok, Req, Opts}.

maybe_sub(<<"POST">>, true, Req0) ->
    {ok, Body, Req} = read_body(Req0),
    try jiffy:decode(Body) of
	Data ->
	    lager:info("json ok"),
	    response(Req, handle(to_dict(Data)))
    catch
	_:_ ->
	    lager:info("json error"),
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

handle(#{<<"url">> := Url, <<"topic">> := Topic}) ->
    db:attach(Url, Topic),
    ok;

handle(_) ->
    lager:error("subscribtion failed: missing url").


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

%%%
%%
%%%

to_dict({List}) ->
    to_dict(List, #{}).

to_dict([], D) ->
    D;

to_dict([{Key, Value} | Tail], Dict) ->
    lager:info("Added key ~p: ~p to ~p", [Key, Value, Dict]),
    to_dict(Tail, Dict#{Key => Value}).
