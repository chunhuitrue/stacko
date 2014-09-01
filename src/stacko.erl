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

-export([listen/2]).
-export([close/1]).
-export([accept/1]).

-export([init_script/0]).
-export([release_conf/0]).

-export([if_up/1]).
-export([if_down/1]).
-export([arp/0]).
-export([ifcfg/3]).
-export([route/1]).
-export([route/2]).
-export([route/6]).
-export([ping/1]).
-export([netstat/0]).

-export([mac_to_binary/1]).
-export([get_ip_from_nic/1]).
-export([get_num_ip/1]).
-export([checksum/1]).
-export([cyc_inc_32/1]).
-export([cyc_inc_16/1]).
-export([make_eth_packet/4]).
-export([make_ip_packet/6]).
-export([nic_mac/1]).
-export([milli_second/0]).

-export([test_arp/0]).
-export([test_ping/0]).
-export([test_no_ping/0]).
-export([test_server/0]).



test_server() ->
    {ok, Socket} = stacko:listen(80, []),
    io:format("listen socket: ~p~n", [Socket]).


listen(Port, Options) ->
    tcp:listen(Port, Options).


close(Socket) ->
    tcp:close(Socket).


accept(Socket) ->
    tcp:accept(Socket).


init_script() ->
    if_up(p2p1),
    if_up(p7p1),

    ifcfg({192, 168, 1, 9}, {255, 255, 255, 0}, p2p1),
    ifcfg({192, 168, 1, 8}, {255, 255, 255, 0}, p2p1),
    ifcfg({192, 168, 1, 7}, {255, 255, 255, 0}, p2p1),
    ifcfg({192, 168, 1, 6}, {255, 255, 255, 0}, p2p1),
    ifcfg({10, 10, 1, 9}, {255, 255, 255, 0}, p7p1),
    ifcfg({10, 10, 1, 8}, {255, 255, 255, 0}, p7p1),
    
    route(add, {0, 0, 0, 0}, {192, 168, 1, 1}, {0, 0, 0, 0}, [$G], p2p1),
    route(add, {10, 10, 1, 0}, null, {255, 255, 255, 0}, [], p7p1),
    route(add, {192, 168, 1, 0}, null, {255, 255, 255, 0}, [], p2p1).


release_conf() ->
    if_down(p2p1),
    if_down(p7p1).


netstat() ->
    tcp:netstat().


if_up(NicName) ->
    {Index, MAC, HwType, MTU} =  nif:nic_up(NicName),
    tables:insert_nic(NicName, Index, MAC, HwType, MTU).


if_down(NicName) ->
    ok = nif:nic_down(NicName),
    true = tables:del_nic(NicName),
    ok.


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


ifcfg(Ip, Mask, Nic) ->
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


ping(IP) ->
    icmp:ping_pid(self()),
    icmp:ping_loop(5, IP, 0, 0).


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



