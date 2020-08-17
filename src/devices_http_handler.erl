%%%-------------------------------------------------------------------
%%% @author Anton Frolov
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. ago 2020 17:37
%%%-------------------------------------------------------------------
-module(devices_http_handler).
-author("Anton Frolov").

-include("device.hrl").

%% API
-export([init/2]).

init(HttpRequest, Options) ->
  #{method := Method} = HttpRequest,
  HttpReply = handle_request(Method, HttpRequest, Options),
  {ok, HttpReply, Options}.

handle_request(<<"POST">>, HttpRequest, _Options) ->
  {ok, Body, Req} = cowboy_req:read_body(HttpRequest),
  #{<<"name">> := Name} = jiffy:decode(Body, [return_maps]),
  {ok, Uuid} = device_storage:create_device(Name),
  cowboy_req:reply(200, #{}, Uuid, Req);

handle_request(<<"GET">>, HttpRequest, _Options) ->
  {ok, ListDevices} = device_storage:list_devices(),
  Response = jiffy:encode(devices_to_map(ListDevices)),
  cowboy_req:reply(200,
    #{<<"Content-Type">> => <<"application/json">>},
    Response, HttpRequest).

devices_to_map(List) ->
  lists:map(fun (Device) -> #{
                  <<"id">> => Device#device.id,
                  <<"name">> => Device#device.name,
                  <<"created">> => Device#device.created
  } end, List).
