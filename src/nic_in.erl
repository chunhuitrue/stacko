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

-export([start_link/1]).
-export([nic_read/1]).


start_link(DispatcherNum) ->
    Pid = spawn_link(?MODULE, nic_read, [DispatcherNum]),
    register(nicread, Pid),
    {ok, Pid}.


nic_read(DispatcherNum) ->
    nic_in(DispatcherNum, DispatcherNum).
nic_in(DispatcherNum, Acc) when Acc < 0 ->
    nic_in(DispatcherNum, DispatcherNum);
nic_in(DispatcherNum, Acc) when Acc >= 0 ->
    case nif:nic_recv() of
        {error, eagain} ->
            timer:sleep(5);
        {error, ebadf} ->
            timer:sleep(1000);
        {error, _Reason} ->
            ok;
        Packet ->
            DispName = list_to_atom(atom_to_list(dispatcher) ++ integer_to_list(Acc)),
            dispatcher:to_dispatcher(DispName, Packet)
    end,
    nic_in(DispatcherNum, Acc - 1).
