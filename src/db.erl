-module(db).
-behaviour(gen_server).

-vsn("0.0.1").

-define(SERVER, ?MODULE).

-export([start_link/0]).
-export([post/2,
	 attach/2,
	 reset/0,
	 debug/1]).

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

post(Topic, Item) ->
    gen_server:cast(?SERVER, {post, Topic, Item}).

attach(Client, Topic) ->
    gen_server:cast(?SERVER, {attach, Client, Topic}).

reset() ->
    gen_server:cast(?SERVER, reset).

%%%
%%
%%%

-record(state, {items = [],
		clients = #{}}).

%

init(_) ->
    {ok, #state{}}.

%

handle_call(What, _From, State) ->
    {reply, {ok, What}, State}.

%

handle_cast({post, Topic, Item}, State0) ->
    State = post(Topic, Item, State0),
    {noreply, State};

handle_cast({attach, ClientUrl, Topic}, State0) ->
    {ok, Client} = clients_sup:start_client(ClientUrl),
    send_history(Client, Topic, State0),
    State = attach(Client, Topic, State0),
    {noreply, State};

handle_cast(reset, State) ->
    {noreply, reset(State)};

handle_cast(_What, State) ->
    {noreply, State}.

%

handle_info(_What, State) ->
    {noreply, State}.

%

terminate(Reason, _State) ->
    lager:error("~p terminates because of ~p", [?SERVER, Reason]),
    ok.

%

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%
%%
%%%

post(Topic, Item, #state{items=Items, clients=Clients}) ->
    lager:info("attempt post ~p ~p", [Topic, Item]),
    post(Topic, Item, maps:find(Topic, Clients)),
    #state{items=[{Topic, Item} | Items], clients=Clients};

post(Topic, _Item, error) ->
    lager:info("no clients for topic ~p", [Topic]),
    ok;

post(Topic, Item, {ok, Clients}) ->
    lager:info("clients for topic ~p ~p", [Topic, Clients]),
    post(Topic, Item, Clients);

post(Topic, Item, []) ->
    lager:info("db done. no more clients for ~p ~p", [Topic, Item]),
    ok;

post(Topic, Item, [Client | Rest]) ->
    lager:info("send to client ~p", [Client]),
    client:send(Client, Item),
    post(Topic, Item, Rest).

%%%
%%
%%%

send_history(_Client, _Topic, []) ->
    ok;

send_history(Client, Topic, [{Topic, Data} | Tail]) ->
    client:send(Client, Data),
    send_history(Client, Topic, Tail);

send_history(Client, Topic, [_H | T]) ->
    send_history(Client, Topic, T);

send_history(Client, Topic, State=#state{items=Items}) ->
    send_history(Client, Topic, Items),
    State.

%%%
%%
%%%

attach(Client, Topic, ClientMap, error) ->
    maps:put(Topic, [Client], ClientMap);

attach(Client, Topic, ClientMap, {ok, Clients}) ->
    maps:put(Topic, [Client | Clients], ClientMap).

attach(Client, Topic, State=#state{clients=ClientMap0}) ->
    ClientMap = attach(Client, Topic, ClientMap0, maps:find(Topic, ClientMap0)),
    State#state{clients=ClientMap}.

%%%
%%
%%%

reset(_State) ->
    #state{}.

%%%
%%
%%%

debug_items([]) ->
    ok;

debug_items([H | T]) ->
    lager:info("db item: ~p", [H]),
    debug_items(T).

debug_clients([]) ->
    ok;

debug_clients([{Key, Value} | T]) ->
    lager:info("topic clients:\t\t~p:~p", [Key, Value]),
    debug_clients(T);

debug_clients(ClientMap) ->
    debug_clients(maps:to_list(ClientMap)).

debug(#state{items=Items, clients=Clients}) ->
    debug_items(Items),
    debug_clients(Clients).
