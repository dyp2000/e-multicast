%%%-------------------------------------------------------------------
%%% @author Dennis Y. Parygin
%%% @copyright (C) 2014-2017, Telemetric Solutions ltd.
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(emc).
-author("Dennis Y. Parygin").
-email("dyp2000@mail.ru").

-define(APPS, [
	emc_app
]).

%% API
-export([start/0, stop/0]).

%%%===================================================================
%%% API functions
%%%===================================================================

start() ->
	ensure_started(?APPS).

stop() ->
	stop_apps(lists:reverse(?APPS)).

%% ===================================================================
%% Internal functions
%% ===================================================================

ensure_started([]) -> ok;
ensure_started([App | Apps]) ->
	case application:start(App) of
		ok ->
			ensure_started(Apps);
		{error, {already_started, App}} ->
			io:fwrite("error start apps"),
			ensure_started(Apps)
	end.

stop_apps([]) -> ok;
stop_apps([App | Apps]) ->
	io:fwrite("Application [~p] stoped.~n", [App]),
	application:stop(App),
	stop_apps(Apps).
