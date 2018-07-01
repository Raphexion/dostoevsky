-module(client).
-behaviour(gen_server).

-define(SERVER, ?MODULE).

-define(HEADERS, []).
-define(CONTENT_TYPE, "text/plain").
-define(TIMEOUT_S, 2).

-define(DEFAULT_HTTPC, httpc).

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

init(Url) when is_binary(Url)->
    init(binary:bin_to_list(Url));

init(Url) ->
    lager:info("new client with url: ~p", [Url]),
    {ok, #{url => Url}}.

handle_call(What, _From, State) ->
    {reply, {ok, What}, State}.

handle_cast({send, Payload}, State) ->
    {ok, Url} = maps:find(url, State),
    lager:info("client attempts to send ~p to ~p", [Payload, Url]),
    case httpc:request(post, {Url, ?HEADERS, ?CONTENT_TYPE, Payload}, [{timeout, timer:seconds(?TIMEOUT_S)}], []) of
	{error, timeout} ->
	    lager:error("client timeout ~p", [Url]);

	{error, {failed_connect, _}} ->
	    lager:error("client failed to connect ~p", [Url]);

	{ok, {{_, 400, Reason}, _, _}} ->
	    lager:error("client failed with 400 ~p", [Reason]);

	Res ->
	    lager:error("client ~p", [Res])
    end,
    {noreply, State};

handle_cast(_What, State) ->
    {noreply, State}.

handle_info(_What, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
