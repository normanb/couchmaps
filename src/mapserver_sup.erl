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

-module(mapserver_sup).
-behaviour(supervisor).

%% External exports
-export([start_link/1]).

%% supervisor callbacks
-export([init/1]).

-include("couch_db.hrl").

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

start_link(MapServExec) ->
    supervisor:start_link(mapserver_sup, MapServExec).

%%%----------------------------------------------------------------------
%%% Callback functions from supervisor
%%%----------------------------------------------------------------------

init(MapServExec) ->
    % set the restart strategy to be generous, mapserv.c bails a lot, allow 10 restarts in 1 secod
    {ok, {{one_for_one, 10, 1},
          [{mapserver, {mapserver, start_link, [MapServExec]},
            permanent, 10, worker, [mapserver]}]}}.
