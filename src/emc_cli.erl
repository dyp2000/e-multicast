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

%% API
-export([
	start/0,
	receiver/1
]).

%%%===================================================================
%%% API functions
%%%===================================================================

start() ->
	Res = gen_udp:open(?MCAST_PORT, [
		binary,
		{active, false},
		{reuseaddr, true},
		{ip, ?MCAST_GROUP},
		{multicast_ttl, 1},
		{multicast_loop, false},
		{add_membership, {?MCAST_GROUP, {0,0,0,0}}}
	]),

	case Res of
		{ok, Sock} ->
			receiver(Sock);
		{error, Err} ->
			io:format("Error: {~p,~p}~n", [error, Err]),
			error
	end.

receiver(Socket) ->
	% io:format("Recvd. Socket: ~p~n", [Socket]),
  	case gen_udp:recv(Socket, 0, 5000) of
  		{error, Reason} ->
  			io:format("Error: ~p~n", [Reason]);
  			% halt();
  		{ok, {_Addr, _Port, Pkt}} ->
  			% io:format("Addr: ~p Port: ~p Packet: ~p~n", [Addr, Port, Pkt])
  			io:format("Packet received. Size: ~p~n", [byte_size(Pkt)])
  	end,
	receiver(Socket).

