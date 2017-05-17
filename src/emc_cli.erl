%%%-------------------------------------------------------------------
%%% @author Dennis Y. Parygin
%%% @copyright (C) 2014, Telemetric Solutions ltd.
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(emc_cli).
-author("Dennis Y. Parygin").
-email("dyp2000@mail.ru").

-define(MCAST_GROUP,  {224,2,2,4}).
-define(MCAST_PORT, 1234).
-define(utf8(Chars), unicode:characters_to_list(Chars)).
-define(chrBin(Chars), unicode:characters_to_binary(Chars)).

%% API
-export([
	start/0,
	receiver/5
]).

%%%===================================================================
%%% API functions
%%%===================================================================

start() ->
	{Group, Crc32} = get_metadata(),
	start_over(),
	Res = gen_udp:open(?MCAST_PORT, [
		binary,
		{active, false},
		{reuseaddr, true},
		{ip, Group},
		{multicast_ttl, 1},
		{multicast_loop, false},
		{add_membership, {Group, {0,0,0,0}}}
	]),
	case Res of
		{ok, Sock} ->
			Opts = inet:getopts(Sock, [buffer, recbuf]),
			io:format("Opts: ~p~n", [Opts]),
			{ok, IoDev} = file:open("./received.file", [write, binary]),
			receiver(Sock, 0, Crc32, IoDev, <<>>);
		{error, Err} ->
			io:format("Error: {~p,~p}~n", [error, Err]),
			error
	end.

receiver(Socket, Sz, Crc32, IoDev, Bin) ->
  	{NewSz, NewBin} = case gen_udp:recv(Socket, 0, 5000) of
  		{error, Reason} ->
  			io:format("Error: ~p~n", [Reason]),
  			{0, <<>>};
  		{ok, {_Addr, _Port, Pkt}} ->
  			case Pkt of
  				<<"eof">> ->
  					io:format("~nFile full received~nSize: ~p~n", [Sz]),
  					file:write(IoDev, Bin),
  					file:sync(IoDev),
  					file:close(IoDev),
  					halt(),
  					{0, <<>>};
  				<<"start_over">> ->
  					io:format("START OVER...~n"),
  					{0, <<>>};
  				B ->
  					{Sz+byte_size(B), iolist_to_binary([Bin, B])}
  			end
  	end,
  	% io:format("Recieved size: ~p~n", [NewSz]),
	receiver(Socket, NewSz, Crc32, IoDev, NewBin).

get_metadata() ->
	{ok, Args} = init:get_argument(url),
	Url = lists:flatten(Args),
	io:fwrite("Url: ~p~n", [Url]),

	inets:start(),
	{ok, {_Status, _Header, Data}} = httpc:request(Url),
	inets:stop(),
	{[{<<"result">>, {Meta}}]} = jsonx:decode(list_to_binary(Data)),
	io:fwrite("~p~n", [Meta]),
	
	{ok, Group} = inet:parse_address(?utf8(proplists:get_value(<<"group">>, Meta))),
	{Group, proplists:get_value(<<"crc32">>, Meta)}.

start_over() ->
	{ok, Args} = init:get_argument(url),
	Url = lists:flatten(Args),
	{ok, {Scheme, _, Host, Port, _, _}} = http_uri:parse(Url),
	Url2 = lists:flatten(io_lib:fwrite("~p\://~s\:~p/data/start_over", [Scheme, Host, Port])),
	io:fwrite("Url2: ~s~n", [Url2]),
	inets:start(),
	{ok, {_Status, _Header, _Data}} = httpc:request(Url2),
	inets:stop().

