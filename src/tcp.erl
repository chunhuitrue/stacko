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



-module(tcp).

-include("stacko.hrl").
-export([netstat/0]).

-export([listen/2,
         accept/1,
         close/1]).

-export([port_is_listening/1,
         checksum/1,
         decode_packet/1,
         decode_arp/2,
         decode_ip/2,
         decode_icmp/2,
         decode_tcp/2,
         build_ip_pak/6,
         nic_mac/1,
         build_eth_pak/4]).


print_listen('$end_of_table') ->
    ok;
print_listen(First) ->
    [{Port, Pid}] = tables:lookup_listen(First),
    io:format("~p    *:~p                      *:*                LISTEN~n", [Pid, Port]),
    print_listen(ets:next(tcp_listen_table, First)).


print_stack({ForeignIP, ForeignPort, LocalIP, LocalPort}, Pid) ->
    case catch tcp_stack:query_state(Pid) of 
        {'EXIT', {noproc, _}} ->
            ok;
        {state, State} ->
            io:format("~p   ~p:~p      ~p:~p    ~p~n", 
                      [Pid, ForeignIP, ForeignPort, LocalIP, LocalPort, State]);
        _ ->
            ok
    end.


netstat() ->
    io:format("pid         local address             foreign address     state~n"),
    print_listen(ets:first(tcp_listen_table)),
    ets:foldl(fun({Key, Val}, ok) ->
                      print_stack(Key, Val),
                      ok
              end,
              ok,
              tcp_stack_table).


listen(Port, _Options) ->
    case catch gen_server:call(tcp_port_mgr, {assign_tcp_port, Port}) of
        {ok, Port} ->
            Backlog = 3,
            tcp_listen_sup:start_child(Port, Backlog, self());
        {'EXIT', {noproc, _}} ->
            {error, noproc};
        Ret ->
            Ret
    end.
    

accept(ListenSocket) ->
    case catch gen_server:call(ListenSocket, accept, infinity) of
        {'EXIT', {noproc, _}} ->
            {error, closed};
        {ok, StackPid} ->
            gen_server:cast(StackPid, {userpid, self()}),
            {ok, StackPid};
        _Ret ->
            error
    end.


close(Socket) ->
    gen_server:call(Socket, {close, self()}, infinity).


port_is_listening(Port) ->
    case tables:lookup_listen(Port) of
        [] ->
            {false, null};
        [{Port, Pid}] ->
            {true, Pid}
    end.


checksum(Date) ->
    16#FFFF - makesum(Date).
    

makesum(Data) ->
    lists:foldl(fun compl/2, 0, [W || <<W:16>> <= Data]).


compl(N) when N =< 16#FFFF -> N;
compl(N) -> (N band 16#FFFF) + (N bsr 16).
compl(N,S) -> compl(N+S).


build_ip_pak(SrcIP, DstIP, ID, Flags, Protocol, Payload) ->
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


%% build_eth_pak(SrcMAC, DstMAC, Type, Payload) ->
%%     Packet = <<DstMAC:48/bits, SrcMAC:48/bits, Type:16/integer-unsigned-big, 
%%                Payload/bits>>,
%%     PacketSize = byte_size(Packet),

%%     if PacketSize < ?MINI_ETH_FRAME ->
%%             PadSize = ?MINI_ETH_FRAME - PacketSize,
%%             Pad = <<0:(PadSize * 8)>>,
%%             <<Packet/bits, Pad/bits>>;
%%        true ->
%%             Packet
%%     end.

build_eth_pak(SrcMAC, DstMAC, Type, Payload) ->
    <<DstMAC:48/bits, SrcMAC:48/bits, Type:16/integer-unsigned-big, 
      Payload/bits>>.


nic_mac(NicName) ->
    [{_NicName, _Index, MAC, _HwType, _MTU}] = tables:lookup_nic(NicName),
    MAC.


decode_tcp(_TcpPacket, Pak) ->
    Pak.


decode_icmp(_IcmpPacket, Pak) ->
    Pak.


decode_ip(IpPacket, Pak) ->
    <<Version:4, Packet/bits>> = IpPacket, 
    case Version of
        ?IPV4 ->
            <<HeaderLen:4, _TOS:8, TotalLen:16/integer-unsigned-big,
              _ID:16, _Flg:3, _FragOffset:13,
              _TTL:8, Protocol:8, _Checksum:16,  
              Sip1:8, Sip2:8, Sip3:8, Sip4:8,
              Dip1:8, Dip2:8, Dip3:8, Dip4:8,
              IpPayload/binary>> = Packet,
            
            Pak2 = Pak#pak{ip_version = Version, 
                           ip_header_len = HeaderLen, 
                           ip_total_len = TotalLen,
                           ip_protocol = Protocol, 
                           sip = {Sip1, Sip2, Sip3, Sip4},
                           dip = {Dip1, Dip2, Dip3, Dip4}},

            case Protocol of 
                ?PROT_ICMP ->
                    decode_icmp(IpPayload, Pak2);
                ?PROT_TCP ->
                    decode_tcp(IpPayload, Pak2)
            end;
        _ ->
            Pak
    end.


decode_arp(_ArpPacket, Pak) ->
    Pak.


decode_packet(Packet) ->
    <<Dmac1:8, Dmac2:8, Dmac3:8, Dmac4:8, Dmac5:8, Dmac6:8, 
      Smac1:8, Smac2:8, Smac3:8, Smac4:8, Smac5:8, Smac6:8,
      EthType:16/integer-unsigned-big,
      EthPayload/binary>> = Packet,

    Pak = #pak{dmac = {Dmac1, Dmac2, Dmac3, Dmac4, Dmac5, Dmac6},
                 smac = {Smac1, Smac2, Smac3, Smac4, Smac5, Smac6},
                 eth_type = EthType},

    case EthType of
        ?TYPE_ARP ->
            decode_arp(EthPayload, Pak);
        ?TYPE_IP ->
            decode_ip(EthPayload, Pak);
        _ ->
            Pak
    end.

    
