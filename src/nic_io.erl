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


-module(nic_io).

-export([agent_in/0]).
-export([agent_out/0]).


agent_in() ->
    case packet:read_clt() of
        {ok, Packet} -> send(Packet)
    end,
    agent_in().

agent_out() ->
    receive
        Packet when erlang:is_binary(Packet) -> packet:write_clt(Packet);
        _Other -> _Other
    end,
    agent_out().
    
send(Packet) ->
    Packet,
    timer:sleep(500),
    io:format("in nif_in!~n",[]).
    
