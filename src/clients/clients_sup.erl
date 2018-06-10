%%%-------------------------------------------------------------------
%% @doc dostoevsky top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(clients_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% API

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    ClientSpec = {client, {client, start_link, []},
		  temporary, 2000, worker, [client]},
    {ok, {{simple_one_for_one, 1, 1}, [ClientSpec]}}.

%%====================================================================
%% Internal functions
%%====================================================================
