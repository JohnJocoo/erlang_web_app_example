%%%-------------------------------------------------------------------
%% @doc web_app public API
%% @end
%%%-------------------------------------------------------------------

-module(web_app_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    web_app_sup:start_link(),
    start_http_server().

stop(_State) ->
    ok.

%% internal functions

start_http_server() ->
    Dispatch = cowboy_router:compile([
        {'_', [{<<"/status">>, hello_world, []},
               {<<"/devices">>, devices_http_handler, []}]}
    ]),
    cowboy:start_clear(http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ).
