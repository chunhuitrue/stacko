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


-module(nic_out).

-behaviour(gen_server).
-export([init/1]).
-export([start_link/0]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-export([send/2]).



init([]) ->
    {ok, null}.


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


handle_cast({Index, Packet}, State) ->
    %% 需要处理非阻塞写当前没有buffer的错误，需要等待和再次尝试
    %% 只能在这里等待，不能再 .c 中等待，否则阻塞。
    Res = nif:nic_send(Index, Packet),
    Res,
    %% io:format("send a packet ~w~n", [Res]),
    {noreply, State}.


handle_info(_Request, State) ->
    {noreply, State}.


handle_call(_Request, _Rrom, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_Oldv, State, _Extra) ->
    {ok, State}.


send(NicIndex, Packet) ->
    gen_server:cast(?MODULE, {NicIndex, Packet}).
