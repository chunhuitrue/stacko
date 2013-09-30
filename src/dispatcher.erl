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


-module(dispatcher).

-behaviour(gen_server).
-export([init/1]).
-export([start_link/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-export([to_dispatcher/2]).


init([Name]) ->
    {ok, Name}.


start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name], []).


handle_cast({Nic, Packet}, StateName) ->
    if bit_size(Packet) >= 112 ->
            <<_Dmac:48, _Smac:48, Type:16/integer-unsigned-big, _Rest/binary>> = Packet,
            case Type  of
                16#0806 ->                      % arp
                    arp:to_arp({Nic, Packet});
                16#0800 ->                      % ip
                    %% io:format("get a ip  packet!~n"),
                    %% test(Packet),
                    ok;
                _ ->
                    ok
            end
    end,
    {noreply, StateName}.


handle_call(_Request, _Rrom, StateName) ->
    {noreply, StateName}.


handle_info(_Request, StateName) ->
    {noreply, StateName}.


terminate(_Reason, _STate) ->
    ok.


code_change(_Oldv, StateName, _Extra) ->
    {ok, StateName}.


to_dispatcher(DispName, Res) ->
    gen_server:cast(DispName, Res).


test(Packet) ->
    %% bits
    <<DA:8, DB:8, DC:8, DD:8, DE:8, DF:8, A:8, B:8, C:8, D:8, E:8, F:8, 
      Type:16/integer-unsigned-big, _Rest/binary>> = Packet,

    Dmac = <<DA, DB, DC, DD, DE, DF>>,
    Smac = <<A, B, C, D, E, F>>,
    %% SelfMac = stacko:mac_to_binary([08, 00, 27, 48, c5, c8]),
    SelfMac = <<16#08, 16#00, 16#27, 16#fc, 16#a2, 16#60>>,
    %% Hostmac = <<16,154,221,169,20,250>>,

    io:format("Type: ~w~n", [Type]),
    io:format("SelfMac: ~w ~w~n", [self(), SelfMac]),
    %% io:format("HostMac: ~w~n", [Hostmac]),
    io:format("Smac: ~w ~w~n", [self(), Smac]),
    %% io:format("Smac2: ~w ~w~n", [self(), {A, B, C, D, E, F}]),
    io:format("Dmac: ~w~n~n", [Dmac]),
    io:format("Dmac2: ~w~n~n", [{DA, DB, DC, DD, DE, DF}]),

    case Smac of
        SelfMac ->
            io:format("get a sendout packet!~n"),
            nic_out:send(p2p1, Packet);
        _ ->
            ok
    end.
    
