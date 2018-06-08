-module(db).
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

post(Topic, Data) ->
    gen_server:cast(?SERVER, {post, Topic, Data}).

init(_) ->
    {ok, #{}}.

handle_call(What, _From, State) ->
    {reply, {ok, What}, State}.

handle_cast({post, Topic, Data}, State0) ->
    State = add(State0, Topic, Data),
    {noreply, State};

handle_cast(_What, State) ->
    {noreply, State}.

handle_info(_What, State) ->
    {noreply, state}.

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

add(State, Topic, Data) ->
    add(State, Topic, Data, maps:find(Topic, State)).

add(State, Topic, Data, {ok, Items}) ->
    lager:info("Add data:   ~p", [Data]),
    maps:put(Topic, [Data | Items], State);

add(State, Topic, Data, error) ->
    lager:info("New topic:  ~p", [Topic]),
    lager:info("First data: ~p", [Data]),
    maps:put(Topic, [Data], State).
