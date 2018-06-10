-module(poster_p).

-include_lib("eunit/include/eunit.hrl").

-define(HEADERS, []).
-define(CONTENT_TYPE, "application/json").

-export([default_sender/1,
	 mock_sender/1,
	 client_status/2,
	 send_item_to_clients/2,
	 send_item_to_clients/3,
	 send_items_to_client/2,
	 send_items_to_client/3]).

%%%
%%
%%%
send_item_to_clients(Item, Clients) ->
    send_item_to_clients(fun default_sender/1, Item, Clients).

send_item_to_clients(Sender, Item, Clients) ->
    send(Sender, [{Item, Client} || Client <- Clients]).

%%%
%%
%%%

send_items_to_client(Items, Client) ->
    send_items_to_client(fun default_sender/1, Items, Client).

send_items_to_client(Sender, Items, Client) ->
    send(Sender, [{Item, Client} || Item <- Items]).

%%%
%%
%%%

client_status(Client, {ok, {{_, 200, _}, _, _}}) ->
    {Client, ok};

client_status(Client, {ok, {{_, 404, _}, _, _}}) ->
    {Client, not_found};

client_status(Client, Blob) ->
    {Client, Blob}.

%%%
%%
%%%

default_sender({Item, Client}) when is_binary(Client) ->
    default_sender({Item, binary_to_list(Client)});

default_sender({Item, Client}) ->
    spawn(fun() ->
		  httpc:request(post, {Client, ?HEADERS, ?CONTENT_TYPE, Item}, [], [])
	  end).

mock_sender({Item, Client}) ->
    {send, {Item, Client}}.

%%%
%%
%%%

send(Sender, IC) ->
    send(Sender, IC, []).

send(_Sender, [], Acc) ->
    Acc;

send(Sender, [H | T], Acc) ->
    send(Sender, T, [Sender(H) | Acc]).

%%%
%%
%%%

send_item_to_clients_1_test() ->
    Res = send_item_to_clients(fun mock_sender/1, item, [client]),
    ?assert(Res =:= [{send, {item, client}}]).

send_item_to_clients_2_test() ->
    Res = send_item_to_clients(fun mock_sender/1, item, [client1, client2]),
    ?assert(Res =:= [{send, {item, client2}}, {send, {item, client1}}]).

send_item_to_clients_3_test() ->
    Res = send_item_to_clients(fun mock_sender/1, item, [client1, client2, client3]),
    ?assert(Res =:= [{send, {item, client3}}, {send, {item, client2}}, {send, {item, client1}}]).

%

send_items_to_client_1_test() ->
    Res = send_items_to_client(fun mock_sender/1, [item], client),
    ?assert(Res =:= [{send, {item, client}}]).

send_items_to_client_2_test() ->
    Res = send_items_to_client(fun mock_sender/1, [item1, item2], client),
    ?assert(Res =:= [{send, {item2, client}},
		     {send, {item1, client}}]).
