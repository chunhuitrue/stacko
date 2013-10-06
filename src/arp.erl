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
-export([arp_find/1]).
-export([get_dst_mac/1]).
-export([test_request/0]).


init([]) ->
    erlang:send_after(?ARP_TIMEOUT, self(), timeout),
    erlang:send_after(?ARP_REFRESH_TIME, self(), refresh),
    {ok, null}.


start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).


%% HwType 1
%% ProtType 0x0800
%% HardSize 6
%% ProtSize 4
%% Op arp request 1. arp replay 2. rarp request 3. rarp replay 4.
handle_cast(Packet, _State) ->
    %% io:format("arp get a packet. PacketSize: ~w~n", [byte_size(Packet)]),
    <<_DstMAC:48/bits, _SrcMAC:48/bits, 
      Type:16/integer-unsigned-big, 
      HwType:16/integer-unsigned-big, ProtType:16/integer-unsigned-big,
      HardSize:8/integer-unsigned-big, ProtSize:8/integer-unsigned-big,
      Op:16/integer-unsigned-big,
      SenderMAC:48/bits, SenderIP:32/bits, _TargetMAC:48/bits, TargetIP:32/bits,
      _Rest/binary>> = Packet,

    <<A:8, B:8, C:8, D:8>> = TargetIP,
    TargetIPnum = {A, B, C, D},
    <<A1:8, B1:8, C1:8, D1:8>> = SenderIP,
    SenderIPnum = {A1, B1, C1, D1},
    {_MegaSecs, Now, _MicroSecs} = erlang:now(),

    if HwType == ?HW_TYPE_MAC ->
            case tables:lookup_ip(TargetIPnum) of
                [{TargetIPnum, _Mask, NIC}] ->          %is about me
                    if Op == ?OP_ARP_REQUEST ->
                            %% io:format("arp get a packet. it is me. ~w~n", [TargetIPnum]),
                            [{NIC, NicIndex, SelfMAC, SelfHwType, _MTU}] = tables:lookup_nic(NIC),
                            Pad = <<0:8/unit:18>>,
                            RePacket = <<SenderMAC:48/bits, SelfMAC:48/bits,
                                         Type:16/integer-unsigned-big, 
                                         SelfHwType:16/integer-unsigned-big, ProtType:16/integer-unsigned-big,
                                         HardSize:8/integer-unsigned-big, ProtSize:8/integer-unsigned-big,
                                         2:16/integer-unsigned-big,
                                         SelfMAC:48/bits, TargetIP:32/bits, 
                                         SenderMAC:48/bits, SenderIP:32/bits,
                                         Pad/bits>>,
                            nic_out:send(NicIndex, RePacket);
                       Op == ?OP_ARP_REPLAY ->
                            %% io:format("arp get a replay. ~n"),
                            tables:insert_arp(SenderIPnum, HwType, SenderMAC, NIC, Now)
                    end;
                [] ->                                   %not about me
                    ok
            end
    end,
    {noreply, null}.


handle_call(_Request, _Rrom, _State) ->
    {noreply, null}.


handle_info(timeout, _State) ->
    del_timeout(),
    erlang:send_after(?ARP_TIMEOUT, self(), timeout),
    {noreply, null};
handle_info(refresh, _State) ->
    refresh(),
    erlang:send_after(?ARP_REFRESH_TIME, self(), refresh),
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

    Pad = <<0:8/unit:18>>,
    Packet = <<DstMAC:48/bits, SelfMAC:48/bits,
               ?TYPE_ARP:16/integer-unsigned-big,
               HwType:16/integer-unsigned-big, ?PROT_TYPE_IP:16/integer-unsigned-big,
               ?MAC_SIZE:8/integer-unsigned-big, ?IP_SIZE:8/integer-unsigned-big,
               ?OP_ARP_REQUEST:16/integer-unsigned-big,
               SelfMAC:48/bits, SenderIP:32/bits, TargetMAC:48/bits, TargetIPbit:32/bits,
               Pad/bits>>,
    nic_out:send(Index, Packet).


arp_find(IP) ->
    case tables:lookup_arp(IP) of
        [{IP, HwType, MAC, NIC, _Time}] ->
            {_MegaSecs, Now, _MicroSecs} = erlang:now(),
            tables:insert_arp(IP, HwType, MAC, NIC, Now),
            MAC;
        []  ->
            null
    end.


test_request() ->
    arp:arp_request({192,168,1,8}, {192,168,1,11}, p2p1).


del_timeout('$end_of_table', _Now) ->
    ok;
del_timeout(First, Now) ->
    [{IP, _HwType, _MAC, _NIC, Time}] = tables:lookup_arp(First), 
    if (Now - Time) >= ?ARP_TIMEOUT_SECOND ->
            tables:del_arp(IP);
       (Now - Time) < ?ARP_TIMEOUT_SECOND ->
            del_timeout(ets:next(arp_table, First), Now)
    end.
del_timeout() ->
    {_MegaSecs, Now, _MicroSecs} = erlang:now(),
    del_timeout(ets:first(arp_table), Now).
    

refresh('$end_of_table', _Now, []) ->
    ok;
refresh('$end_of_table', _Now, [{TargetIP, _HwType, _MAC, NIC, _Time}]) ->
    arp_request(stacko:get_ip_from_nic(NIC), TargetIP, NIC);
refresh(First, Now, [{Acc_IP, Acc_HwType, Acc_MAC, Acc_NIC, Acc_Time}]) ->
    [{IP, HwType, MAC, NIC, Time}] = tables:lookup_arp(First),
    Next = ets:next(arp_table, First),
    if (Now - Time) < (Now - Acc_Time) ->
            refresh(Next, Now, [{IP, HwType, MAC, NIC, Time}]);
       (Now - Time) >= (Now - Acc_Time) ->
            refresh(Next, Now, [{Acc_IP, Acc_HwType, Acc_MAC, Acc_NIC, Acc_Time}])
    end.
refresh() ->
    {_MegaSecs, Now, _MicroSecs} = erlang:now(),
    First = ets:first(arp_table),
    Acc = tables:lookup_arp(First),
    refresh(First, Now, Acc).


get_mac(_IP, _SelfIP, _NICName, 2) ->
    null;
get_mac(IP, SelfIP, NICName, 1) ->
    arp_request(SelfIP, IP, NICName),
    timer:sleep(50).
get_mac(IP, SelfIP, NICName) ->
    case arp_find(IP) of
        null ->
            arp_request(SelfIP, IP, NICName),
            get_mac(IP, SelfIP, NICName, 1),
            timer:sleep(50);
        MAC ->
            MAC
    end.


get_dst_mac(DstIP) ->
    case tables:find_route(DstIP) of
        {direct, NICName} ->
            [{_Name, NICIndex, _MAC, _HwType, _MTU}] = tables:lookup_nic(NICName),
            NICIndex;
        {gateway, Gateway, NICName} ->
            [{_Name, NICIndex, _MAC, _HwType, _MTU}] = tables:lookup_nic(NICName),
            {get_mac(Gateway, tables:get_ip_from_nic(NICName), NICName), NICIndex}
    end.



