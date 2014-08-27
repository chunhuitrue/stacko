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


-module(tcp_port_res).

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
    tables:create_listen(),
    {ok, null}.


handle_cast(_Request, _State) ->
    {noreply, null}.


port_is_listening(Port) ->
    case tables:lookup_listen(Port) of
        [{Port, _Pid}] ->
            true;
        [] ->
            false
    end.


handle_call({listen, Port, _Backlog, _UserPid}, _From, _State) when Port < 0; Port > 65535 ->
    {reply, {error, badport}, _State};

handle_call({listen, Port, Backlog, UserPid}, _From, _State) ->
    case port_is_listening(Port) of
        true ->
            {reply, {error, port_listening}, _State};
        false ->
            {ok, ListenPid} = tcp_listen_sup:start_child(Port, Backlog, UserPid),
            erlang:monitor(process, ListenPid),
            tables:insert_listen(Port, ListenPid),
            {reply, {ok, ListenPid}, _State}
    end.


handle_info({'DOWN', _Ref, process, Pid, _Reason}, _State) ->
    io:format("listen process is down. pid: ~p~n", [Pid]),
    tables:del_listen(Pid),
    {noreply, _State}.


terminate(_Reason, _STate) ->
    ok.


code_change(_Oldv, _State, _Extra) ->
    {ok, null}.


