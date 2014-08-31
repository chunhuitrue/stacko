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



-module(tcp_stack).

-behaviour(gen_server).
-export([init/1]).
-export([start_link/1]).
-export([handle_cast/2]).
-export([handle_call/3]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-export([to_tcp_stack/2]).

-record(state, {listenpid}).



%% remote address: Sip Sport
%% local address: Dip Dport
start_link(ListenPid) ->
    gen_server:start_link(?MODULE, [ListenPid], []).


init([ListenPid]) ->
    {ok, #state{listenpid = ListenPid}}.


handle_cast({syn, _Packet}, State) ->
    io:format("tcp_stack: ~p get a syn packet.~n", [self()]),
    {noreply, State}.


handle_call(_Request, _Rrom, State) ->
    {noreply, State}.


handle_info(_Request, State) ->
    {noreply, State}.


terminate(_Reason, _STate) ->
    ok.


code_change(_Oldv, _State, _Extra) ->
    {ok, _State}.


to_tcp_stack(Packet, Pid) ->
    gen_server:cast(Pid, Packet).
