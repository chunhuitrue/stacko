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


-behaviour(gen_server).
-export([init/1]).
-export([start_link/0]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-export([read_packet/0]).
-export([nic_read/0]).



init([]) ->
    {ok, null}.


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


handle_cast(_Request, State) ->
    {noreply, State}.


handle_call(read, _From, _State) ->
    Res = nic_read(),
    {reply, Res, _State}.
%% handle_call({test, Time}, _From, _State) ->
%%     timer:sleep(Time),
%%     io:format("time out!!~n", []),
%%     {reply, ok, _State};
%% handle_call({test_die}, _From, _State) ->
%%     2 = 3,
%%     {reply, ok, _State}.


handle_info(_Request, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_Oldv, State, _Extra) ->
    {ok, State}.


read_packet() ->
    gen_server:call(?MODULE, read).


nic_read() ->
    case nif:nic_recv() of
        {error, eagain} ->
            %% io:format("nic_read error eagain~n", []),
            timer:sleep(20),
            nic_read();
        {error, ebadf} ->
            %% io:format("nic_read error ebadf~n", []),
            {error, ebadf};
        {error, Reason} ->
            %% io:format("nic_read error reason ~p ~n", [Reason]),
            %% timer:sleep(100),
            {error, Reason};
        Packet ->
            %% io:format("nic_read Packet~n", []),
            {ok, Packet}
    end.

