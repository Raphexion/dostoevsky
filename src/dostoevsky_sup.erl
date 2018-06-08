%%%-------------------------------------------------------------------
%% @doc dostoevsky top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(dostoevsky_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    Db = {
      db,
      {db, start_link, []},
      permanent, 5000, worker, [db]},
    Poster = {
      poster,
      {poster, start_link, []},
      permanent, 5000, worker, [poster]},

    Children = [Db, Poster],
    {ok, { {one_for_all, 0, 1}, Children} }.

%%====================================================================
%% Internal functions
%%====================================================================
