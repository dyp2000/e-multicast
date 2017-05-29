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
	get_host/0
]).

%%%===================================================================
%%% API functions
%%%===================================================================

start() ->
	inets:start(),
	case get_metadata() of
		{Group, FileName, Crc32} ->
			start_over(),
			Res = gen_udp:open(?MCAST_PORT, [
				binary,
				{active, true},
				{reuseaddr, true},
				{ip, Group},
				{multicast_ttl, 1},
				{multicast_loop, false},
				{add_membership, {Group, {0,0,0,0}}}
			]),
			case Res of
				{ok, Sock} ->
					inet:setopts(Sock, [{recbuf, 8*1024}]),
					Opts = inet:getopts(Sock, [buffer, recbuf]),
					io:format("Opts: ~p~n", [Opts]),
					{ok, IoDev} = file:open(FileName, [write, binary]),
					loop(Sock, 0, FileName, Crc32, IoDev, [], 0, 0, []);
				{error, Err} ->
					io:format("Error: {~p,~p}~n", [error, Err]),
					error
			end;
	{error, Reason} ->
		io:format("ERROR: ~p~n", [Reason]),
		halt()
	end.

loop(Socket, Sz, FileName, Crc32, IoDev, Acc, Cnt, Pi, LPC) ->
	receive
		{udp, Socket, _, _, Pkt} ->
			{Ci, B} = parse_pkt(Pkt),
			Lost = lost(Pi, Ci),
			request_lost(Lost),
			I = if
				Ci < Pi -> 
					io:format("Recvd lost: ~p~n", [Ci]),
					Pi;
				true -> Ci
			end,
			loop(Socket, Sz+byte_size(Pkt), FileName, Crc32, IoDev, [{Ci, B}|Acc], Cnt+1, I, [Lost|LPC])

	after 1000 ->
		io:format("Принято ~p пакетов~n", [Cnt]),
		io:format("Потеряны пакеты: ~p~n", [lists:sort(lists:flatten(LPC))]),
		Bin = prepare_bin(Acc),
		io:format("Sz: ~p~n", [byte_size(Bin)]),
		file:write(IoDev, Bin),
		file:sync(IoDev),
		file:close(IoDev),
		io:format("Результат проверки CRC32: ~p~n", [check_crc(FileName, Crc32)]),
		halt()
	end.

prepare_bin(Packets) ->	iolist_to_binary([B || {_I, B} <- lists:sort(Packets)]).

parse_pkt(Bin) ->
	<<Ci:32/integer, Data/binary>> = Bin,
	{Ci, Data}.

lost(Pi, Ci) -> lost(Pi, Ci, Ci - Pi).
lost(Pi, Ci, Step) when (Step > 1) -> 
	Lost = lists:seq(Pi+1, Ci-1, 1),
	io:format("Lost: ~p~n", [Lost]),
	Lost;
lost(_, _, _) -> [].

get_metadata() ->
	{ok, Args} = init:get_argument(url),
	Url = lists:flatten(Args),
	io:fwrite("Url: ~p~n", [Url]),

	case httpc:request(Url) of
		{ok, {_Status, _Header, Data}} ->
			{[{<<"result">>, {Meta}}]} = jsonx:decode(list_to_binary(Data)),
			io:fwrite("~p~n", [Meta]),
			{ok, Group} = inet:parse_address(?utf8(proplists:get_value(<<"group">>, Meta))),
			{Group, proplists:get_value(<<"filename">>, Meta), proplists:get_value(<<"crc32">>, Meta)};
		{error, Reason} ->
			{error, Reason}
	end.

start_over() ->
	{Scheme, Host, Port} = get_host(),
	Url2 = lists:flatten(io_lib:fwrite("~p\://~s\:~p/start_over", [Scheme, Host, Port])),
	{ok, {_Status, _Header, _Data}} = httpc:request(Url2).

check_crc(FileName, OrgCrc32) ->
	{ok, B} = file:read_file("./" ++ FileName),
	Crc32 = erlang:crc32(B),
	OrgCrc32 == Crc32.

get_host() ->
	{ok, Args} = init:get_argument(url),
	Url = lists:flatten(Args),
	{ok, {Scheme, _, Host, Port, _, _}} = http_uri:parse(Url),
	{Scheme, Host, Port}.

request_lost([]) -> ok;
request_lost(Lost) ->
	io:format("Запросить потерянные пакеты: ~p~n", [Lost]),
	{Scheme, Host, Port} = get_host(),
	URL = lists:flatten(io_lib:fwrite("~p\://~s\:~p/req_lost", [Scheme, Host, Port])),
	{ok, _} = httpc:request(post, {URL, [], "multipart/form-data", jsonx:encode(Lost)}, [], []).
