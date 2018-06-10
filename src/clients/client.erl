-module(client).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-export([start_link/1]).
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-export([add_client/1]).

add_client(Url) ->
    supervisor:start_child(clients_sup, [Url]).

start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

init(Url) ->
    lager:info("New client ~p", [Url]),
    {ok, #{url => Url}}.

handle_call(What, _From, State) ->
    {reply, {ok, What}, State}.

handle_cast(_What, State) ->
    {noreply, State}.

handle_info(_What, State) ->
    {noreply, State}.

terminate(Reason, _State) ->
    % lager:error("~p terminates because of ~p", [?SERVER, Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
