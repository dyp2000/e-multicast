-module(emc_app).

-behaviour(application).

%% Application callbacks
-export([
	start/2, 
	stop/1,
	custom_404_hook/4
]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	create_tables(),
	init_cowboy(),
	emc_sup:start_link().

stop(_State) ->
	ok.

%% ===================================================================

custom_404_hook(404, Headers, <<>>, Req) ->
	{ok, Body} = file:read_file("./html/error-page.html"),
	Headers2 = lists:keyreplace(<<"content-length">>, 1, Headers, {<<"content-length">>, integer_to_list(byte_size(Body))}),
	{ok, Req2} = cowboy_req:reply(404, Headers2, Body, Req),
	Req2;
custom_404_hook(_, _, _, Req) ->
	Req.

%% ===================================================================

init_cowboy() ->
	%% Config and Start COWBOY
	RoutersWeb = routes_web(),
	DispatchWeb = cowboy_router:compile(RoutersWeb),
	HttpPort = port(http_port),
	TransOptsWeb = [{port, HttpPort}],
	ProtoOptsWeb = [
		{env, [{dispatch, DispatchWeb}]},
		{onresponse, fun ?MODULE:custom_404_hook/4}
	],
	WebRes = cowboy:start_http(web, 32, TransOptsWeb, ProtoOptsWeb),
	io:fwrite("~nWeb Server started: ~p Port: ~p~n", [WebRes, HttpPort]).

%%%===================================================================
%%% Internal functions
%%%===================================================================

routes_web() ->
	[
		{'_', 
			[
				{"/get_meta", emc_ajax, []},
				{"/start_over", emc_ajax, []},
				{"/req_lost", emc_ajax, []},
				{"/", cowboy_static, {file, "./html/index.html"}},
				{"/[...]", cowboy_static, {dir, "./html"}}
			]
		}
	].

port(http_port) -> 
	application:get_env(emc, http_port, 8088).

create_tables() ->
	ets:new(emc, [named_table, set, public]).

