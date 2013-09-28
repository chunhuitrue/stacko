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
-export([start_link/3]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-export([nic_in/2]).
-export([test/1]).


init([NameIn, Socket, DispatcherNum]) ->
    Name = list_to_atom(atom_to_list(NameIn) ++ "read"),
    register(Name, spawn_link(nic_in, nic_in, [Socket, DispatcherNum])),
    {ok, null}.


nic_in(Socket, DispatcherNum) ->
    nic_in(Socket, DispatcherNum, DispatcherNum - 1).
nic_in(Socket, DispatcherNum, Acc) when Acc >= 0 ->
    case nif:read_nic(Socket) of
        {error, eagain} ->
            timer:sleep(5);
        {error, _Reason} ->
            ok;
        Packet ->
            Name = list_to_atom(atom_to_list(dispatcher) ++ integer_to_list(Acc)),
            dispatcher:to_dispatcher(Name, Packet)
    end,
    nic_in(Socket, DispatcherNum, Acc - 1);
nic_in(Socket, DispatcherNum, Acc) when Acc < 0 ->
    nic_in(Socket, DispatcherNum, DispatcherNum - 1).


test(N) ->
    x_read(nif:open_nic(p2p1), N).


x_read(Socket, Num) when Num >= 0 ->
    nif:read_nic(Socket),
    %% io:format("~w resd and to!~n",[self()]),
    dispatcher:to_dispatcher(dispatcher0, ok),
    x_read(Socket, Num - 1);
x_read(_Socket, Num) when Num < 0 ->
    ok.


start_link(NameIn, Socket, DispatcherNum) ->
    gen_server:start_link({local, NameIn}, ?MODULE, [NameIn, Socket, DispatcherNum], []).


handle_cast(_Request, _State) ->
    {noreply, null}.


handle_info(_Request, _State) ->
    {noreply, null}.


handle_call(_Request, _Rrom, _State) ->
    {noreply, null}.


terminate(_Reason, _State) ->
    ok.


code_change(_Oldv, _State, _Extra) ->
    {ok, null}.
