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
-export([start_link/2]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-export([nicread/1]).


init([DispatcherNum]) ->
    register(nicread, spawn_link(nic_in, nicread, [DispatcherNum])),
    {ok, null}.


start_link(Name, DispatcherNum) ->
    gen_server:start_link({local, Name}, ?MODULE, [DispatcherNum], []).


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


nicread(DispatcherNum) ->
    timer:sleep(10000),
    nicread(DispatcherNum),
    ok.


%% nic_in(Socket, DispatcherNum) ->
%%     nic_in(Socket, DispatcherNum, DispatcherNum - 1).
%% nic_in(Socket, DispatcherNum, Acc) when Acc >= 0 ->
%%     case nif:read_nic(Socket) of
%%         {error, eagain} ->
%%             timer:sleep(5);
%%         {error, _Reason} ->
%%             ok;
%%         Res ->
%%             io:format("get a packet ~w~n", [self()]),

%%             DispName = list_to_atom(atom_to_list(dispatcher) ++ integer_to_list(Acc)),
%%             dispatcher:to_dispatcher(DispName, Res)
%%     end,
%%     nic_in(Socket, DispatcherNum, Acc - 1);
%% nic_in(Socket, DispatcherNum, Acc) when Acc < 0 ->
%%     nic_in(Socket, DispatcherNum, DispatcherNum - 1).


