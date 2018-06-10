-module(reset_handler).

-export([init/2]).

init(Req, Opts) ->
    db:reset(),
    cowboy_req:reply(200, Req),
    {ok, Req, Opts}.
