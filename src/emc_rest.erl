%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author Dennis Y. Parygin
%%% @copyright (C) 2015
%%% @doc
%%% REST requests handler
%%% @end
%%%-------------------------------------------------------------------
-module(emc_rest).
-author("Dennis Y. Parygin").
-email("dyp2000@mail.ru").

-define(HEAD_JS_CONTENT, {<<"content-type">>, <<"application/javascript; charset=utf-8">>}).
-define(GET, <<"GET">>).
-define(POST, <<"POST">>).
-define(get_meta, <<"get_meta">>).
-define(start_over, <<"start_over">>).
% -define(user_info, <<"user_info">>).
% -define(get_persons, <<"get_persons">>).
-define(AJAX_REPLY(Status, Reply, State), State#ajax_state.handler ! {ajax_reply, Status, Reply}).

-export([
	init/3,
	init_rest/2,
	allowed_methods/2,
	content_types_provided/2,
	content_types_accepted/2,
	commands_handler/2
]).

init(_Transport, _Req, _State) ->
	{upgrade, protocol, cowboy_rest}.

init_rest(Req, State) ->
	{ok, Req, State}.

allowed_methods(Req, State) ->
	{[?GET, ?POST], Req, State}.

content_types_provided(Req, State) ->
	{[
		{<<"application/json">>, commands_handler}
	], Req, State}.

content_types_accepted(Req, State) ->
	{[
		{<<"application/json">>, commands_handler}
	], Req, State}.

commands_handler(Req, State) ->
	{Command, _} = cowboy_req:binding(command, Req),
	case cowboy_req:body_qs(Req) of
		{ok, Post, Req2} ->
			command(Req2, Command, Post),
			{halt, Req2, State};
		{badlength, Req2} ->
			{halt, Req2, State};
		{error, Reason} ->
			lager:error("Error: ~p", [Reason]),
			reply(bad_request, Req),
			{halt, Req, State}
	end.

reply(Data, Req) ->
	case Data of
		{get_meta, Result} -> 
			cowboy_req:reply(200, [?HEAD_JS_CONTENT], jsonx:encode([{result, Result}]), Req);
		{start_over, Result} ->
			cowboy_req:reply(200, [?HEAD_JS_CONTENT], jsonx:encode([{result, Result}]), Req);
		% {user_info, Result} ->
		% 	cowboy_req:reply(200, [?HEAD_JS_CONTENT], jsonx:encode([{result, Result}]), Req);
		% {get_persons, Result} ->
		% 	cowboy_req:reply(200, [?HEAD_JS_CONTENT], jsonx:encode([{result, Result}]), Req);
		bad_request -> 
			cowboy_req:reply(400, [?HEAD_JS_CONTENT], "Bad request", Req)
	end.

command(Req, ?get_meta, _Params) -> reply(emc_ajax:cmd_get_meta(), Req);
command(Req, ?start_over, _Params) -> reply(emc_ajax:cmd_start_over(), Req);
% command(Req, ?user_info, Params) -> reply(tmsNetflowAjax:cmd_user_info(Params), Req);
% command(Req, ?get_persons, Params) -> reply(tmsNetflowAjax:cmd_get_persons(Params), Req);
command(Req, _, _) -> reply(bad_request, Req).
