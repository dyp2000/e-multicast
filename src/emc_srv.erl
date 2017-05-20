%%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author Dennis Y. Parygin
%%% @copyright (C) 2014, Telemetric Solutions Ltd.
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(emc_srv).
-author("Dennis Y. Parygin").
-email("dyp2000@mail.ru").

-behaviour(gen_server).

-include_lib("kernel/include/file.hrl").

%% API
-export([
	start_link/0,
	send_file/5,
	send_lost/2
]).

%% emc_srv callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-define(SERVER, ?MODULE).

-define(MCAST_GROUP, {224,2,2,4}).
-define(MCAST_PORT, 1234).
-define(PKTSZ, 1468).

-define(utf8(Chars), unicode:characters_to_list(Chars)).
-define(chrBin(Chars), unicode:characters_to_binary(Chars)).

-record(state, {
	mSocket,
	send_timer,
	filepath,
	multicast_group,
	speed,
	iodev,
	crc32,
	file_size,
	stime,
	sender,
	pktmap
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
	{ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% emc_srv callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
	{ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term()} | ignore).
init([]) ->
	io:format("Server start...~n"),
	case parse_args() of
		{[], _, _} ->
			{stop, error_input_args};
		{_, {}, _} ->
			{stop, error_input_args};
		{_, _, 0} ->
			{stop, error_input_args};
		{File, Group, Speed} ->
			{ok, Sock} = gen_udp:open(0, [
				binary, 
				{active, false}, 
				{multicast_ttl, 1}
				% {multicast_loop,true}
			]),
			io:format("Socket: ~p~n", [Sock]),
			Opts = inet:getopts(Sock, [buffer, sndbuf]),
			io:format("Opts: ~p~n", [Opts]),
			{ok, B} = file:read_file(File),
			Crc32 = erlang:crc32(B),
			io:format("file crc32: ~p~n", [Crc32]),
			PktMap = pkt_map(B, 0, 0, []),
			Sz = filelib:file_size(File),
			{ok, IoDev} = file:open(File, [read, binary]),
			Time = (trunc(1000/(Speed/1472))),
			io:format("send time: ~p~n", [Time]),
			ets:insert(emc, {pos, {bof, 0}}),
			ets:insert(emc, {start_over, false}),
			Timer = erlang:send_after(500, ?SERVER, send_file),
			{ok, #state{
				mSocket = Sock, 
				send_timer = Timer, 
				filepath = File, 
				multicast_group = Group, 
				speed = Speed, 
				iodev = IoDev, 
				crc32 = Crc32,
				stime = Time,
				file_size = Sz,
				pktmap = PktMap}
			}
	end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: #state{}) ->
	{reply, Reply :: term(), NewState :: #state{}} |
	{reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
	{noreply, NewState :: #state{}} |
	{noreply, NewState :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
	{stop, Reason :: term(), NewState :: #state{}}).

handle_call(get_metadata, _From, State) ->
	Reply = [
		{group, ?chrBin(inet:ntoa(State#state.multicast_group))},
		{filename, filename:basename(State#state.filepath)},
		{filesize, State#state.file_size},
		{crc32, State#state.crc32}
	],
	{reply, Reply, State};

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
	{noreply, NewState :: #state{}} |
	{noreply, NewState :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #state{}}).

handle_cast(start_over, State) ->
	io:format("Передача файла сначала~n"),
	exit(State#state.sender, start_over),
	ets:insert(emc, {pos, {bof, 0}}),
	ets:insert(emc, {start_over, true}),
	Timer = erlang:send_after(500, ?SERVER, send_file),
	{noreply, State#state{sender = 0, send_timer = Timer}};

handle_cast({send_lost, Lost}, State) ->
	spawn(emc_srv, send_lost, [Lost, State]),
	{noreply, State};

handle_cast(_Request, State) ->
	io:format("Unknown request: ~p~n", [_Request]),
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
	{noreply, NewState :: #state{}} |
	{noreply, NewState :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #state{}}).

handle_info(send_file, State) ->
	erlang:cancel_timer(State#state.send_timer),
	Pid = spawn_link(emc_srv, send_file, [
		State#state.mSocket, 
		State#state.multicast_group, 
		State#state.iodev, 
		State#state.stime, 
		0
	]),
	{noreply, State#state{sender = Pid}};

handle_info(_Info, State) ->
	io:format("Unknown info: ~p~n", [_Info]),
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a emc_srv when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the emc_srv terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
	State :: #state{}) -> term()).
terminate(_Reason, _State) ->
 	ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{}, Extra :: term()) ->
	{ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

parse_args() ->
	File = case init:get_argument(infile) of
	 	{ok, Args} -> 
	 		lists:flatten(Args);
	 	error -> 
	 		[]
	end,
	io:format("File: ~p~n", [File]),

	Grp = case init:get_argument(mcgrp) of
	 	{ok, Args2} -> 
	 		{ok, Ip} = inet:parse_address(lists:flatten(Args2)),
	 		Ip;
	 	error -> 
	 		{}
	end,
	io:format("Multicast group: ~p~n", [Grp]),

	Speed = case init:get_argument(speed) of
	 	{ok, Args3} -> 
	 		list_to_integer(lists:flatten(Args3));
	 	error -> 
	 		0
	end,
	io:format("Speed: ~p KByte/sec~n", [round(Speed/1024)]),
	{File, Grp, Speed}.

make_pkt(Ci, Bin) -> <<Ci:32/integer, Bin/binary>>.

send_file(Socket, Group, IoDev, Time, C) ->
	[{pos, Pos}|_] = ets:match_object(emc, {pos, '$1'}),
	case file:pread(IoDev, Pos, ?PKTSZ) of
	 	{ok, Bin} ->
			ok = gen_udp:send(Socket, Group, ?MCAST_PORT, make_pkt(C, Bin)),
			{bof, P} = Pos,
			ets:insert(emc, {pos, {bof,  P + ?PKTSZ}}),
			timer:sleep(trunc(Time)),
			send_file(Socket, Group, IoDev, Time, C+1);	 		
		eof ->
			io:format("<EOF>~n"),
			io:format("Отправлено ~p пакетов~n", [C]),
			ets:insert(emc, {pos, {bof, 0}}),
			timer:sleep(5000),
	 		gen_server:cast(?SERVER, start_over);
		{error, _Reason} ->
			error
	end.

pkt_map(<<>>, _, _, Acc) -> 
	lists:sort(Acc);
pkt_map(<<Pkt:?PKTSZ/binary, Tail/binary>>, Idx, Pos, Acc) ->
	pkt_map(Tail, Idx+1,  Pos + byte_size(Pkt), [{Idx, Pos}|Acc]);
pkt_map(<<Pkt/binary>>, Idx, Pos, Acc) ->
	pkt_map(<<>>, Idx+1, Pos + byte_size(Pkt), [{Idx, Pos}|Acc]).

send_lost(Lost, State) ->
	{ok, Sock} = gen_udp:open(0),
	lists:foreach(
		fun(I) -> 
			Pos = proplists:get_value(I, State#state.pktmap),
			send_lost(Sock, State#state.multicast_group, State#state.iodev, I, Pos)
		end, 
	Lost).

send_lost(Socket, Group, IoDev, Idx, Pos) ->
	io:format("send lost: ~p pos: ~p~n",[Idx, Pos]),
	case file:pread(IoDev, {bof, Pos}, ?PKTSZ) of
		{ok, Bin} ->
			ok = gen_udp:send(Socket, Group, ?MCAST_PORT, make_pkt(Idx, Bin));
		eof ->
			eof;
		{error, _Reason} ->
			error
	end.
