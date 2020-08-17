%%%-------------------------------------------------------------------
%%% @author Anton Frolov
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. ago 2020 16:43
%%%-------------------------------------------------------------------
-module(device_storage).
-author("Anton Frolov").

-include("device.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0,
         create_device/1,
         list_devices/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(device_storage_state, {devices}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

create_device(Name) ->
  gen_server:call(?SERVER, {create_device, Name}).

list_devices() ->
  gen_server:call(?SERVER, list_devices).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #device_storage_state{}} | {ok, State :: #device_storage_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  {ok, #device_storage_state{devices = #{}}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #device_storage_state{}) ->
  {reply, Reply :: term(), NewState :: #device_storage_state{}} |
  {reply, Reply :: term(), NewState :: #device_storage_state{}, timeout() | hibernate} |
  {noreply, NewState :: #device_storage_state{}} |
  {noreply, NewState :: #device_storage_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #device_storage_state{}} |
  {stop, Reason :: term(), NewState :: #device_storage_state{}}).
handle_call({create_device, Name}, _From, State = #device_storage_state{devices = Devices}) ->
  Uuid = uuid:to_string(uuid:uuid1()),
  Created = iso8601:format(calendar:universal_time()),
  Device = #device{id = Uuid, name = Name, created = Created},
  NewDevices = Devices#{Uuid => Device},
  {reply, {ok, Uuid}, State#device_storage_state{devices = NewDevices}};

handle_call(list_devices, _From, State = #device_storage_state{devices = Devices}) ->
  ListDevices = maps:values(Devices),
  {reply, {ok, ListDevices}, State};

handle_call(_Request, _From, State = #device_storage_state{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #device_storage_state{}) ->
  {noreply, NewState :: #device_storage_state{}} |
  {noreply, NewState :: #device_storage_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #device_storage_state{}}).
handle_cast(_Request, State = #device_storage_state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #device_storage_state{}) ->
  {noreply, NewState :: #device_storage_state{}} |
  {noreply, NewState :: #device_storage_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #device_storage_state{}}).
handle_info(_Info, State = #device_storage_state{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #device_storage_state{}) -> term()).
terminate(_Reason, _State = #device_storage_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #device_storage_state{},
    Extra :: term()) ->
  {ok, NewState :: #device_storage_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #device_storage_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
