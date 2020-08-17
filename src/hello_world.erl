%%%-------------------------------------------------------------------
%%% @author Anton Frolov
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. ago 2020 17:02
%%%-------------------------------------------------------------------
-module(hello_world).
-author("Anton Frolov").

%% API
-export([init/2]).

init(HttpRequest, Options) ->
  HttpReply = cowboy_req:reply(200, #{}, <<"Hello world!">>, HttpRequest),
  {ok, HttpReply, Options}.