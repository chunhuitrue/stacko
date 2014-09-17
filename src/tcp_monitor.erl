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



-module(tcp_monitor).

-inlcude("stacko.hrl").

-behaviour(gen_server).
-export([init/1]).
-export([start_link/0]).
-export([handle_cast/2]).
-export([handle_call/3]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).



start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init([]) ->
    {ok, null}.


handle_cast({monitor_me, Pid}, _State) ->
    erlang:monitor(process, Pid),
    {noreply, null}.


handle_call(_Request, _From, _State) ->
    {noreply, _State}.


handle_info({'DOWN', _Ref, process, _Pid, normal}, _State) ->
    %% io:format("tcp_monitor: pid: ~p exit do nothing~n", [Pid]),
    {noreply, _State};

handle_info({'DOWN', _Ref, process, Pid, _Reason}, _State) ->
    %% io:format("tcp_monitor: pid: ~p exit do clean~n", [Pid]),
    case tables:find_listen_port(Pid) of
        null ->
            %% io:format("tcp_monitor: pid: ~p is not listen pid~n", [Pid]),
            case tables:find_stack(Pid) of
                [{{RemoteIP, RemotePort, LocalIP, LocalPort}, Pid}] ->
                    tables:del_stack(RemoteIP, RemotePort, LocalIP, LocalPort),
                    tables:release_tcp_port(LocalPort);
                [] ->
                    error
            end;
        Port ->
            %% io:format("clean listen table and tcp port table~n"),
            tables:del_listen(Port),
            tables:release_tcp_port(Port)
    end,
    {noreply, _State}.


terminate(_Reason, _STate) ->
    ok.


code_change(_Oldv, _State, _Extra) ->
    {ok, null}.


