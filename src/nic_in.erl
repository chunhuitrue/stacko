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


-module(nic_in).

-include("head.hrl").

-export([start_link/0]).



start_link() ->
    Pid = spawn_link(fun() -> start([]) end),
    register(?MODULE, Pid),
    {ok, Pid}.


start(List) when length(List) < ?DISPATCHER_NUM ->
    receive
        {regist, Pid} ->
            start([Pid | List])
    end;
start(List) when length(List) >= ?DISPATCHER_NUM ->
    loop(List, []).


loop([], Acc) ->
    loop(Acc, []);
loop(PidList, Acc) ->
    case nif:nic_recv() of
        {error, eagain} ->
            %% io:format("nic_read error eagain~n", []),
            timer:sleep(20),
            loop(PidList, Acc);
        {error, ebadf} ->
            %% io:format("nic_read error ebadf~n", []),
            loop(PidList, Acc);
        {error, _Reason} ->
            %% io:format("nic_read error reason ~p ~n", [Reason]),
            %% timer:sleep(100),
            loop(PidList, Acc);
        Packet ->
            %% io:format("nic_read Packet~n", []),
            gen_server:cast(hd(PidList), {packet, Packet}),
            loop(tl(PidList), [hd(PidList) | Acc])
    end.

