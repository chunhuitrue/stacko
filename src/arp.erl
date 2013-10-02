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


-module(arp).

-include("head.hrl").

-behaviour(gen_server).
-export([init/1]).
-export([start_link/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-export([to_arp/1]).
-export([arp_request/3]).
-export([test_request/0]).


init([]) ->
    {ok, null}.


start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).


%% HwType 1
%% ProtType 0x0800
%% HardSize 6
%% ProtSize 4
%% Op arp request 1. arp replay 2. rarp request 3. rarp replay 4.
handle_cast(Packet, _State) ->
    io:format("arp get a packet. PacketSize: ~w~n", [byte_size(Packet)]),
    <<_DstMAC:48/bits, _SrcMAC:48/bits, 
      Type:16/integer-unsigned-big, 
      HwType:16/integer-unsigned-big, ProtType:16/integer-unsigned-big,
      HardSize:8/integer-unsigned-big, ProtSize:8/integer-unsigned-big,
      Op:16/integer-unsigned-big,
      SenderMAC:48/bits, SenderIP:32/bits, _TargetMAC:48/bits, TargetIP:32/bits,
      _Rest/binary>> = Packet,

    case Op of
        ?OP_ARP_REQUEST ->                                    
            <<A:8, B:8, C:8, D:8>> = TargetIP,
            RequestIP = {A, B, C, D},
            case tables:lookup_ip(RequestIP) of
                [{RequestIP, _Mask, Nic}] ->
                    io:format("arp get a packet. it is me. ~w~n", [RequestIP]),
                    [{Nic, NicIndex, SelfMAC, HwType, _MTU}] = tables:lookup_nic(Nic),
                    RePacket = <<SenderMAC:48/bits, SelfMAC:48/bits,
                                 Type:16/integer-unsigned-big, 
                                 HwType:16/integer-unsigned-big, ProtType:16/integer-unsigned-big,
                                 HardSize:8/integer-unsigned-big, ProtSize:8/integer-unsigned-big,
                                 2:16/integer-unsigned-big,
                                 SelfMAC:48/bits, TargetIP:32/bits, 
                                 SenderMAC:48/bits, SenderIP:32/bits>>,
                    nic_out:send(NicIndex, RePacket),
                    Pad = <<0:8/unit:18>>,
                    RePacketPad = <<RePacket/bits, Pad/bits>>,
                    nic_out:send(NicIndex, RePacketPad);
                [] ->
                    io:format("arp get a packet. it is not me. ~w~n", [RequestIP])
            end;
        ?OP_ARP_REPLAY ->
            ok;
        _ ->
            ok
    end,
    {noreply, null}.


handle_call(_Request, _Rrom, _State) ->
    {noreply, null}.


handle_info(_Request, _State) ->
    {noreply, null}.


terminate(_Reason, _STate) ->
    ok.


code_change(_Oldv, _State, _Extra) ->
    {ok, null}.


to_arp(Packet) ->
    gen_server:cast(arp, Packet).


arp_request(SelfIP, TargetIP, NicName) ->
    [{_NicName, Index, SelfMAC, HwType, _MTU}] = tables:lookup_nic(NicName),

    DstMAC = <<16#ff:8, 16#ff:8, 16#ff:8, 16#ff:8, 16#ff:8, 16#ff:8>>,
    TargetMAC = <<0:8/unit:6>>,
    {A, B, C, D} = SelfIP,
    SenderIP = <<A:8, B:8, C:8, D:8>>,
    {A1, B1, C1, D1} = TargetIP,
    TargetIPbit = <<A1:8, B1:8, C1:8, D1:8>>,

    Packet = <<DstMAC:48/bits, SelfMAC:48/bits,
               ?TYPE_ARP:16/integer-unsigned-big,
               HwType:16/integer-unsigned-big, ?PROT_TYPE_IP:16/integer-unsigned-big,
               ?MAC_SIZE:8/integer-unsigned-big, ?IP_SIZE:8/integer-unsigned-big,
               ?OP_ARP_REQUEST:16/integer-unsigned-big,
               SelfMAC:48/bits, SenderIP:32/bits, TargetMAC:48/bits, TargetIPbit:32/bits>>,
    nic_out:send(Index, Packet),

    Pad = <<0:8/unit:18>>,
    PacketPad = <<Packet/bits, Pad/bits>>,
    nic_out:send(Index, PacketPad).


test_request() ->
    arp:arp_request({192,168,1,8}, {192,168,1,11}, p2p1).
