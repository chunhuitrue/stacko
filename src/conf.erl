%% Copyright LiChunhui (chunhui_true@163.com)
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.


-module(conf).

-behaviour(gen_server).
-export([init/1]).
-export([start_link/0]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-define(LOOP_TIME, 1000).


init([]) ->
    erlang:send_after(?LOOP_TIME, self(), watch_conf),
    {ok, null}.


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
    

handle_info(watch_conf, _State) ->
    load_conf(),
    %% erlang:send_after(?LOOP_TIME, self(), watch_conf),
    {noreply, null}.


handle_call(_Request, _Rrom, _State) ->
    {noreply, null}.


handle_cast(_Request, _State) ->
    {noreply, null}.


terminate(_Reason, _STate) ->
    ok.


code_change(_Oldv, _State, _Extra) ->
    {ok, null}.


load_conf() ->
    %% open_nic ....

    %% 10 dispatchers
    Dispatcher_num = 3,
    ip_sup:start_dispatcher(Dispatcher_num - 1),

    tables:insert_ip({192, 168, 1, 12}, {255, 255, 255, 0}, p2p1, [08, 00, 27, fc, a2, 60]),
    tables:insert_ip({10, 10, 1, 12}, {255, 255, 255, 0}, p7p1, [08, 00, 27, 48, c5, c8]),
    arp:acd().


