% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(mapserver).
-behaviour(gen_server).

%% External exports
-export([start_link/1]).

%% API functions
-export([request/3]).

%% gen_server callbacks
-export([init/1, 
         handle_call/3,
         handle_cast/2, 
         handle_info/2,
         code_change/3,
         terminate/2]).

-include("couch_db.hrl").

%% Server state
-record(state, {port, timeout}).

-define(PORT_OPTIONS, [{packet, 4}, exit_status, binary, hide]).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

start_link(MapServExec) ->
    gen_server:start_link({local, ?MODULE}, mapserver, MapServExec, []).

request(Msg, CallBackFunc, FuncState) when is_list(Msg) ->
  request(list_to_binary(Msg), CallBackFunc, FuncState);

request(Msg, CallBackFunc, FuncState) ->   
  gen_server:call(?MODULE, {mapserv, Msg, CallBackFunc, FuncState}).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

init(MapServExec) ->
    process_flag(trap_exit, true),
    Port = open_port({spawn, MapServExec}, ?PORT_OPTIONS),
    Timeout = list_to_integer(couch_config:get(
	"couchdb", "os_process_timeout", "5000")),
    {ok, #state{port = Port, timeout = Timeout}}.

handle_call({mapserv, Msg, CallBackFunc, FuncState}, _From, #state{port = Port, timeout = Timeout} = State) ->
    port_command(Port, Msg),
    case collect_response(Port, Timeout, CallBackFunc, FuncState) of
        {response, Response} -> 
            {reply, Response, State};
        timeout -> 
            {stop, port_timeout, State}
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', Port, Reason}, #state{port = Port} = State) ->
    {stop, {port_terminated, Reason}, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate({port_terminated, _Reason}, _State) ->
    ok;
terminate(_Reason, #state{port = Port} = _State) ->
    port_close(Port).

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

collect_response(Port, Timeout, CallBackFunc, FuncState) ->
    receive
        {'EXIT', Port, Reason} ->
           ?LOG_ERROR("~p~n", [Reason]),
           {response, CallBackFunc(done, FuncState)};
        {Port, {data, <<"CouchDB_Done">>}} ->
           {response, CallBackFunc(done, FuncState)};
        {Port, {data, Data}} ->
           NewState = CallBackFunc(Data, FuncState),
           collect_response(Port, Timeout, CallBackFunc, NewState)
    after Timeout -> 
      timeout
    end.

