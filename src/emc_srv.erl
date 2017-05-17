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
-export([start_link/0]).

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
	stime
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
			{ok, Sock} = gen_udp:open(0, [binary]),
			io:format("Socket: ~p~n", [Sock]),
			
			Opts = inet:getopts(Sock, [buffer, sndbuf]),
			io:format("Opts: ~p~n", [Opts]),

			{ok, B} = file:read_file(File),
			Crc32 = erlang:crc32(B),
			io:format("file crc32: ~p~n", [Crc32]),

			{ok, FileInfo} = file:read_file_info(File),
			Sz = FileInfo#file_info.size,

			{ok, IoDev} = file:open(File, [read, binary]),

			Time = (trunc(1000/(Speed/1500))),
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
				file_size = Sz}
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

handle_cast({send_block, Bin}, State) ->

	[{time, Time}|_] = ets:match_object(emc, {time, $1}),
	send_block(State#state.mSocket, State#state.multicast_group, Time, Bin),

	{noreply, State};

handle_cast(start_over, State) ->
	io:format("Start OVER!~n"),
	ets:insert(emc, {start_over, true}),
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
	[{start_over, StartOver}|_] = ets:match_object(emc, {start_over, '$1'}),

	case StartOver of
		true -> 
			io:format("Start over~n"),
			ets:insert(emc, {pos, {bof, 0}}),
			ets:insert(emc, {start_over, false});
		false -> 
	 		false
	end,

	[{pos, Pos}|_] = ets:match_object(emc, {pos, '$1'}),
	Res = case file:pread(State#state.iodev, Pos, State#state.speed) of
	 	{ok, Bin} ->
	 		send_block(State#state.mSocket, State#state.multicast_group, State#state.stime, Bin),
			{bof, P} = Pos,
			ets:insert(emc, {pos, {bof, P+State#state.speed}});
		eof ->
			io:format("<EOF>~n"),
			ets:insert(emc, {pos, {bof, 0}}),
	 		send_block(State#state.mSocket, State#state.multicast_group, State#state.stime, <<"eof">>);
	 		% halt();
		{error, _Reason} ->
			error
	end,
	case Res of
		error ->
			io:format("file read error"),
			{stop, file_read_error, State};
		_ ->
			Timer = erlang:send_after(1, ?SERVER, send_file),
			{noreply, State#state{send_timer = Timer}}
	end;

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

send_block(_Socket, _Group, _Time, <<>>) ->
	ok;
send_block(Socket, Group, Time, <<Block:1472/binary, Rest/binary>>) ->
	gen_udp:send(Socket, Group, ?MCAST_PORT, Block),
	timer:sleep(Time),
	send_block(Socket, Group, Time, Rest);
send_block(Socket, Group, _Time, <<Rest/binary>>) ->
	gen_udp:send(Socket, Group, ?MCAST_PORT, Rest).

