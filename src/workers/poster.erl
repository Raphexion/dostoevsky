-module(poster).
-behaviour(gen_server).

-define(SERVER, ?MODULE).

-export([start_link/0]).
-export([post/2]).

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

post(Data, Clients) ->
    gen_server:call(?SERVER, {post, Data, Clients}).

init(_) ->
    {ok, []}.

handle_call({post, Data, Clients}, _From, State) ->
    {ok, Res} = post_to_clients(Data, Clients),
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

%%%
%%
%%%

client_status(Client, {ok, {{_, 200, _}, _, _}}) ->
    {Client, ok};

client_status(Client, {ok, {{_, 404, _}, _, _}}) ->
    {Client, not_found};

client_status(Client, Blob) ->
    {Client, Blob}.

post_to_client(_Data, Client) ->
    client_status(Client, httpc:request(Client)).

%
%

post_to_clients(_Data, [], Status) ->
    {ok, Status};

post_to_clients(Data, [H | T], Status) ->
    Res = post_to_client(Data, H),
    post_to_clients(Data, T, [Res | Status]).

%
%

post_to_clients(Data, Clients) ->
    post_to_clients(Data, Clients, []).
