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


-module(stacko).

-include("head.hrl").

-export([nic_up/1]).
-export([nic_down/1]).
-export([arp/0]).
-export([conf_ip/3]).
-export([mac_to_binary/1]).
-export([get_ip_from_nic/1]).
-export([route/1]).
-export([route/2]).
-export([route/6]).
-export([get_num_ip/1]).
-export([checksum/1]).
-export([make_ip_icmp_replay/4]).
-export([cyc_inc_32/1]).
-export([cyc_inc_16/1]).
-export([make_eth_packet/4]).
-export([make_icmp_ping_packet/1]).
-export([ping/1]).
-export([make_ip_packet/6]).
-export([nic_mac/1]).
-export([milli_second/0]).

-export([test_arp/0]).
-export([test_ping/0]).
-export([test_no_ping/0]).


nic_up(NicName) ->
    case nif:nic_up(NicName) of
        {error, Resion} ->
            {error, Resion};
        {Index, MAC, HwType, MTU}  ->
            tables:insert_nic(NicName, Index, MAC, HwType, MTU)
    end.


nic_down(NicName) ->
    nif:nic_down(NicName),
    tables:del_nic(NicName).


arp('$end_of_table') ->
    ok;
arp(First) ->
   [{IP, HwType, MAC, NIC, _Time}] = tables:lookup_arp(First),
    io:format("~w ~w ~w ~w~n", [IP, HwType, MAC, NIC]),
    arp(ets:next(arp_table, First)).
arp() ->
   arp(ets:first(arp_table)).


test_arp() ->
    MAC = <<16#ff:8, 16#ff:8, 16#ff:8, 16#ff:8, 16#ff:8, 16#ff:8>>,
    {_MegaSecs, Now, _MicroSecs} = erlang:now(),
    tables:insert_arp({192, 168, 1, 20}, 1, MAC, p2p1, Now),
    tables:insert_arp({192, 168, 1, 21}, 1, MAC, p2p1, Now),
    tables:insert_arp({192, 168, 1, 22}, 1, MAC, p2p1, Now).


conf_ip(Ip, Mask, Nic) ->
    tables:insert_ip(Ip, Mask, Nic).


mac_to_binary(Mac) ->
    case length(Mac) of
        6 ->
            mac_to_binary(Mac, <<>>);
        _ ->
            badarg
    end.
mac_to_binary([], Acc) ->
    Acc;
mac_to_binary([A | Tail], Acc) ->
    if is_atom(A) ->
            Data2 = list_to_integer(atom_to_list(A), 16),
            mac_to_binary(Tail, list_to_binary([Acc, <<Data2:8>>]));
       is_integer(A) ->
            Data = list_to_integer(integer_to_list(A), 16),
            mac_to_binary(Tail, list_to_binary([Acc, <<Data:8>>]))
    end.


get_ip_from_nic('$end_of_table', _NicName) ->
    null;
get_ip_from_nic(First, NicName) ->
    case tables:lookup_ip(First) of
        [{IP, _Mask, NicName}] ->
            IP;
        _ ->
            get_ip_from_nic(ets:next(ip_table, First), NicName)
    end.
get_ip_from_nic(NicName) ->
    get_ip_from_nic(ets:first(ip_table), NicName).


route('$end_of_table') ->
    ok;
route(First) when is_integer(First)  ->
    Res = ets:lookup(route_table, First),
    io:format("~w~n", [Res]),
    route(ets:next(route_table, First));
route(print) ->
    route(ets:first(route_table)).


route(add, Destiantion, Gateway, Mask, Flag, NIC) ->
    case ets:last(route_table) of
        '$end_of_table' ->
            tables:insert_route(0, Destiantion, Gateway, Mask, Flag, NIC);
        LastIndex ->
            tables:insert_route(LastIndex + 1, Destiantion, Gateway, Mask, Flag, NIC)
    end.


route(del, Num) ->
    tables:del_route(Num).


get_num_ip({A, B, C, D}) ->
    Res = (A * math:pow(2, 24)) + (B * math:pow(2, 16)) + (C * math:pow(2, 8)) + D,
    round(Res).


checksum(Date) ->
    16#FFFF - makesum(Date).
    

makesum(Data) ->
    lists:foldl(fun compl/2, 0, [W || <<W:16>> <= Data]).


compl(N) when N =< 16#FFFF -> N;
compl(N) -> (N band 16#FFFF) + (N bsr 16).
compl(N,S) -> compl(N+S).


make_ip_icmp_replay(SrcIP, DstIP, ID, ICMPPayload) ->
    HeadLen4Byte = 5,
    TotalLenByte = (HeadLen4Byte * 4) + byte_size(ICMPPayload),
    {S1, S2, S3, S4} = SrcIP,
    {D1, D2, D3, D4} = DstIP,

    CRC = stacko:checksum(<<?IPV4:4, HeadLen4Byte:4, 0:8, TotalLenByte:16/integer-unsigned-big,
                            ID:16/integer-unsigned-big, 0:3, 0:13/integer-unsigned-big,
                            64:8, ?PROT_ICMP:8, 0:16/integer-unsigned-big,
                            S1:8, S2:8, S3:8, S4:8,
                            D1:8, D2:8, D3:8, D4:8>>),

    <<?IPV4:4, HeadLen4Byte:4, 0:8, TotalLenByte:16/integer-unsigned-big,
      ID:16/integer-unsigned-big, 0:3, 0:13/integer-unsigned-big,
      64:8, ?PROT_ICMP:8, CRC:16/integer-unsigned-big,
      S1:8, S2:8, S3:8, S4:8,
      D1:8, D2:8, D3:8, D4:8,
      ICMPPayload/bits>>.


cyc_inc_32(Num) ->
    if Num =:= 16#ffffffff ->
            0;
       true ->
            Num + 1
    end.


cyc_inc_16(Num) ->
    if Num =:= 16#ffff ->
            0;
       true ->
            Num + 1
    end.


make_eth_packet(SrcMAC, DstMAC, Type, Payload) ->
    Packet = <<DstMAC:48/bits, SrcMAC:48/bits, Type:16/integer-unsigned-big, 
               Payload/bits>>,
    PacketSize = byte_size(Packet),

    if PacketSize < ?MINI_ETH_FRAME ->
            PadSize = ?MINI_ETH_FRAME - PacketSize,
            Pad = <<0:(PadSize * 8)>>,
            <<Packet/bits, Pad/bits>>;
       true ->
            Packet
    end.


make_icmp_ping_packet(SeqNum) ->
    TimeStamp = milli_second(),
    IcmpID = 2013,
    Pad = <<0:8/unit:52>>,

    CRC = checksum(<<8:8, 0:8, 0:16/integer-unsigned-big,
                     IcmpID:16, SeqNum:16/integer-unsigned-big,
                     TimeStamp:32/integer-unsigned-big,
                     Pad/bits>>),
    <<8:8, 0:8, CRC:16/integer-unsigned-big,
      IcmpID:16/integer-unsigned-big, SeqNum:16/integer-unsigned-big,
      TimeStamp:32/integer-unsigned-big,
      Pad/bits>>.



make_ip_packet(SrcIP, DstIP, ID, Flags, Protocol, Payload) ->
    HeadLen4Byte = 5,
    TotalLenByte = (HeadLen4Byte * 4) + byte_size(Payload),
    {S1, S2, S3, S4} = SrcIP,
    {D1, D2, D3, D4} = DstIP,

    CRC = checksum(<<?IPV4:4, HeadLen4Byte:4, 0:8, TotalLenByte:16/integer-unsigned-big,
                     ID:16/integer-unsigned-big, Flags:3, 0:13/integer-unsigned-big,
                     64:8, Protocol:8, 0:16/integer-unsigned-big,
                     S1:8, S2:8, S3:8, S4:8,
                     D1:8, D2:8, D3:8, D4:8>>),
    <<?IPV4:4, HeadLen4Byte:4, 0:8, TotalLenByte:16/integer-unsigned-big,
      ID:16/integer-unsigned-big, Flags:3, 0:13/integer-unsigned-big,
      64:8, Protocol:8, CRC:16/integer-unsigned-big,
      S1:8, S2:8, S3:8, S4:8,
      D1:8, D2:8, D3:8, D4:8,
      Payload/bits>>.


recv_pang(ReqIcmpSeq) ->
    receive
        {pang, Packet} ->
            <<_Dmac:48, _Smac:48, _Type:16,
              _Version:4, HeadLen4Byte:4, _TOS:8, TotalLenByte:16/integer-unsigned-big,
              _IpID:16, _Flg:3, _FragOff:13,
              TTL:8, _Protocol:8, _CRCIP:16,  
              Sip1:8, Sip2:8, Sip3:8, Sip4:8, 
              Dip1:8, Dip2:8, Dip3:8, Dip4:8, 
              IpPayload/binary>> = Packet,

            SrcIP = {Sip1, Sip2, Sip3, Sip4},
            DstIP = {Dip1, Dip2, Dip3, Dip4},
            HeadLen = HeadLen4Byte * 32,
            TotalLen = TotalLenByte * 8,
            ICMPLen = TotalLen - HeadLen,
            IsMy = tables:is_my_ip(DstIP),
            <<IcmpPack:ICMPLen/bits, _Rest/binary>> = IpPayload,
            <<Type:8, Code:8, _CRC:16, 
              _IcmpID:16, IcmpSeq:16/integer-unsigned-big,
              Timestamp:16/integer-unsigned-big, _Pad/binary>> = IcmpPack,
            CRC = stacko:checksum(IcmpPack),
            UseTime = milli_second() - Timestamp,

            if IsMy =:= true, CRC =:= 0, Type =:=0, Code =:= 0, ReqIcmpSeq =:= IcmpSeq ->
                    io:format("~w bytes from ~w: icmp_seq=~w ttl=~w time=~wms~n", 
                              [ICMPLen, SrcIP, IcmpSeq, TTL, UseTime]);
               true ->
                    ok
            end,
            timer:sleep(1000)
    after 1000 ->
            io:format("Request timeout for icmp_seq ~w~n", [ReqIcmpSeq])
    end.


ping_loop(0, _IP, _IcmpSeq, _IpID) ->
    ok;
ping_loop(Num, IP, IcmpSeq, IpID) ->
    case arp:get_dst_mac2(IP) of
        {error, noroute} ->
            io:format("No route to host~n"),
            timer:sleep(1000),
            ping_loop(Num - 1, IP, cyc_inc_16(IcmpSeq), cyc_inc_32(IpID));
        {error, _, _} ->
            io:format("Host is down~n"),
            timer:sleep(1000),
            ping_loop(Num - 1, IP, cyc_inc_16(IcmpSeq), cyc_inc_32(IpID));
        {DstMAC, NicName, NicIndex} ->
            SrcMAC = nic_mac(NicName),
            SrcIP = get_ip_from_nic(NicName),

            IcmpPack = make_icmp_ping_packet(IcmpSeq),
            IpPack = make_ip_packet(SrcIP, IP, IpID, 0, ?PROT_ICMP, IcmpPack),
            EthPack = make_eth_packet(SrcMAC, DstMAC, ?TYPE_IP, IpPack),
            nic_out:send(NicIndex, EthPack),

            recv_pang(IcmpSeq),
            ping_loop(Num - 1, IP, cyc_inc_16(IcmpSeq), cyc_inc_32(IpID))
    end.


ping(IP) ->
    icmp:ping_pid(self()),
    ping_loop(5, IP, 0, 0).


nic_mac(NicName) ->
    [{_NicName, _Index, MAC, _HwType, _MTU}] = tables:lookup_nic(NicName),
    MAC.


test_ping() ->
    ping({192,168,1,11}).


test_no_ping() ->
    ping({192,168,1,22}).


milli_second() ->
    {_Meg, _S, Micro} = os:timestamp(),
    Micro div 1000.



