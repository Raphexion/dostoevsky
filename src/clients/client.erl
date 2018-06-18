-module(client).
-behaviour(gen_server).

-define(SERVER, ?MODULE).

-define(HEADERS, []).
-define(CONTENT_TYPE, "application/json").

-export([start_link/1,
	 send/2]).

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

%%====================================================================
%% API
%%====================================================================

start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

send(Client, Data) ->
    gen_server:cast(Client, {send, Data}).

%%====================================================================
%% Gen Server
%%====================================================================

init(Url) ->
    {ok, #{url => Url}}.

handle_call(What, _From, State) ->
    {reply, {ok, What}, State}.

handle_cast({send, Payload}, State) ->
    {ok, Url} = maps:find(url, State),
    httpc:request(post, {Url, ?HEADERS, ?CONTENT_TYPE, Payload}, [], []),
    {noreply, State};

handle_cast(_What, State) ->
    {noreply, State}.

handle_info(_What, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
