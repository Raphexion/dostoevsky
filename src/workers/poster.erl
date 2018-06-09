-module(poster).
-behaviour(gen_server).

-define(SERVER, ?MODULE).

-export([start_link/0]).
-export([send_item_to_clients/2,
	 send_items_to_client/2]).

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

%%%
%% API
%%%

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

send_item_to_clients(Item, Clients) ->
    gen_server:call(?SERVER, {send_item_to_clients, Item, Clients}).

send_items_to_client(Items, Client) ->
    gen_server:call(?SERVER, {send_items_to_client, Items, Client}).

%%%
%%
%%%

init(_) ->
    {ok, []}.

handle_call({send_item_to_clients, Item, Clients}, _From, State) ->
    Res = poster_p:send_item_to_clients(Item, Clients),
    {reply, {ok, Res}, State};

handle_call({send_items_to_client, Item, Clients}, _From, State) ->
    Res = poster_p:send_items_to_client(Item, Clients),
    {reply, {ok, Res}, State};

handle_call(What, _From, State) ->
    {reply, {ok, What}, State}.

handle_cast(_What, State) ->
    {noreply, State}.

handle_info(_What, State) ->
    {noreply, State}.

terminate(normal, _State) ->
    ok;

terminate(Reason, _State) ->
    lager:error("terminate ~p", [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    %% No change planned. The function is there for the behaviour,
    %% but will not be used. Only a version on the next
    {ok, State}.
