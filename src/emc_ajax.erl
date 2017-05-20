%% -*- coding: utf-8 -*-
-module(emc_ajax).
-author("Dennis Y. Parygin").
-email("dyp2000@mail.ru").
-behaviour(cowboy_loop_handler).

-record(ajax_state, {handler}).
-define(HEAD_JS_CONTENT, {<<"content-type">>, <<"application/javascript; charset=utf-8">>}).
-define(AJAX_REPLY(Status, Reply, State), State#ajax_state.handler ! {ajax_reply, Status, Reply}).

-export([
	init/3, 
	info/3, 
	terminate/3,
	ajax/4
]).

%%%===================================================================
%%% API
%%%===================================================================

init(_Transport, Req, []) ->
	{Method, Req2} = cowboy_req:method(Req),
	{Path, Req3} = cowboy_req:path(Req2),
	State = #ajax_state{handler = self()},
	self() ! {ajax, Method, Path},
    {loop, Req3, State, hibernate}.

terminate(_Reason, _Req, _State) ->
    ok.

%%%===================================================================
%%% callbacks
%%%===================================================================

-spec(info(any(), Req, State) -> {ok, Req, State} | {loop, Req, State} | {loop, Req, State, hibernate} when Req::cowboy_req:req()).

info({ajax, Method, Path}, Req, State) ->
	{Req2, NewState} = ajax(Method, Path, Req, State),
 	{loop, Req2, NewState, hibernate};

info({ajax_reply, Status, Reply}, Req, State) ->
	case Status of
		200 -> {ok, Req2} = cowboy_req:reply(Status, [?HEAD_JS_CONTENT], Reply, Req);
		400 -> {ok, Req2} = cowboy_req:reply(Status, [?HEAD_JS_CONTENT], Reply, Req);
		_ -> {ok, Req2} = cowboy_req:reply(Status, [], Reply, Req)
	end,
	{ok, Req2, State};

info(Msg, Req, State) ->
	lager:warning("Unknown message ~p with request ~p", [Msg, Req]),
    {loop, Req, State, hibernate}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec(ajax(Method::string(), Path::string(), Req::term(), State::#ajax_state{}) -> {Req::term(), State::#ajax_state{}}).

ajax(<<"GET">>, <<"/get_meta">>, Req, State) ->
	Reply = gen_server:call(emc_srv, get_metadata),
	io:format("Metadata: ~p~n", [Reply]),
	?AJAX_REPLY(200, jsonx:encode([{result, Reply}]), State),
	{Req, State};

ajax(<<"GET">>, <<"/start_over">>, Req, State) ->
	gen_server:cast(emc_srv, start_over),
	?AJAX_REPLY(200, jsonx:encode([{result, ok}]), State),
	{Req, State};

ajax(<<"POST">>, <<"/req_lost">>, Req, State) ->
	case cowboy_req:has_body(Req) of
		false ->
			?AJAX_REPLY(400, <<"Bad AJAX parameters">>, State),
			{Req, State};
		true -> 
			case cowboy_req:body_qs(Req) of
				{ok, PostVals, Req2} ->
					% io:format("Post: ~p~n", [PostVals]),
					[{Bin,_} |_] = PostVals,
					gen_server:cast(emc_srv, {send_lost, jsonx:decode(Bin)}),
					?AJAX_REPLY(200, jsonx:encode([{result, ok}]), State),
					{Req2, State};
				{error, Req2} ->
					?AJAX_REPLY(400, <<"Bad AJAX parameters">>, State),
					{Req2, State}
			end
	end;

ajax(<<"GET">>, _, _Req, State) ->
	?AJAX_REPLY(406, <<"(GET) Not Acceptable">>, State);

ajax(<<"POST">>, _, _Req, State) ->
	?AJAX_REPLY(406, <<"(POST) Not Acceptable">>, State);

ajax(_, _, _Req, State) ->
	%% Method not allowed.
	?AJAX_REPLY(405, <<"Method not allowed">>, State).

