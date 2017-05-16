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

-record(state, {
	mSocket,
	send_timer,
	filepath,
	multicast_group,
	speed,
	iodev,
	pos = {bof, 0},
	send_time
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
	io:format("Запуск сервера...~n"),
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

			{ok, IoDev} = file:open(File, [read, binary]),
			Time = (trunc(1000/(Speed/1500))),
			io:format("send time: ~p~n", [Time]),

			Timer = erlang:send_after(1, ?SERVER, send_file),
			{ok, #state{mSocket = Sock, send_timer = Timer, filepath = File, multicast_group = Group, speed = Speed, iodev = IoDev, send_time = Time}}
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
handle_cast(_Request, State) ->
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
	NewState = case file:pread(State#state.iodev, State#state.pos, State#state.speed) of
		{ok, Bin} ->
			msend(State#state.mSocket, State#state.multicast_group, State#state.send_time, Bin),
			{bof, P} = State#state.pos,
			State#state{pos = {bof, P + State#state.speed+1}};
		eof ->
			io:format("Eof. Start again.\n"),
			State#state{pos = {bof, 0}};
		{error, _Reason} ->
			error
	end,

	case NewState of
		error ->
			{stop, file_read_error, State};
		_ ->
			Timer = erlang:send_after(1, ?SERVER, send_file),
			{noreply, NewState#state{send_timer = Timer}}
	end;

handle_info(_Info, State) ->
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

msend(Socket, Group, Time, <<Block:1472/binary, Rest/binary>>) ->
	gen_udp:send(Socket, Group, ?MCAST_PORT, Block),
	timer:sleep(Time),
	msend(Socket, Group, Time, Rest);
msend(Socket, Group, _Time, <<Rest/binary>>) ->
	gen_udp:send(Socket, Group, ?MCAST_PORT, Rest);
msend(_Socket, _Group, _Time, <<>>) ->
	ok.

