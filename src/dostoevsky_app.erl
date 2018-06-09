%%%-------------------------------------------------------------------
%% @doc dostoevsky public API
%% @end
%%%-------------------------------------------------------------------

-module(dostoevsky_app).

-behaviour(application).

%% Application callbacks
-export([start/2,
	 stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
				      {'_', [
					     {"/pub", pub_handler, []},
					     {"/sub", sub_handler, []},
					     {"/", cowboy_static, {priv_file, dostoevsky, "static/index.html"}},
					     {"/assets/[...]", cowboy_static, {priv_dir, dostoevsky, "static/assets"}}
					    ]}
				      ]),
    {ok, _} = cowboy:start_clear(http, [{port, 7070}], #{
					 env => #{dispatch => Dispatch}
					}),
    dostoevsky_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
