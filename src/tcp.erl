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
         close/1,
         setopts/2]).

-export([port_is_listening/1,
         checksum/1,
         decode_packet/1,
         decode_arp/2,
         decode_ip/2,
         decode_icmp/2,
         decode_tcp/2,
         build_tcp_packet/1,
         build_ip_packet/1,
         build_eth_packet/4,
         build_eth_packet_with_pad/4,
         nic_mac/1]).


print_listen('$end_of_table') ->
    ok;
print_listen(First) ->
    [{Port, Pid}] = tables:lookup_listen(First),
    io:format("~p    *:~p                      *:*                LISTEN~n", [Pid, Port]),
    print_listen(ets:next(stacko_tcp_listen, First)).


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
    print_listen(ets:first(stacko_tcp_listen)),
    ets:foldl(fun({Key, Val}, ok) ->
                      print_stack(Key, Val),
                      ok
              end,
              ok,
              stacko_tcp_stack).


listen(Port, PropList) ->
    case catch gen_server:call(tcp_port_mgr, {assign_tcp_port, Port}) of
        {ok, Port} ->
            tcp_listen_sup:start_child(Port, 
                                       proplists:get_value(backlog, PropList, ?DEFAULT_BACKLOG),
                                       self());
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


setopts(Socket, PropList) ->
    case catch gen_server:call(Socket, 
                               {active, proplists:get_value(active, PropList, true), 
                                self()}) of
        {'EXIT', {noproc, _}} ->
            {error, closed};
        ok ->
            ok
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


compl(N) when N =< 16#FFFF -> N;
compl(N) -> (N band 16#FFFF) + (N bsr 16).
compl(N,S) -> compl(N+S).


makesum(Data) ->
    lists:foldl(fun compl/2, 0, [W || <<W:16>> <= Data]).


checksum(Date) ->
    16#FFFF - makesum(Date).
    

psedu_ip_tcp_header({Sip1, Sip2, Sip3, Sip4}, {Dip1, Dip2, Dip3, Dip4}, TcpLen) ->
    <<Sip1:8, Sip2:8, Sip3:8, Sip4:8,
      Dip1:8, Dip2:8, Dip3:8, Dip4:8,
      0:8, ?PROT_TCP:8, TcpLen:16/integer-unsigned-big>>.


psedu_tcp_packet(Packet) ->
    case byte_size(Packet) rem 2 of
        0 ->
            Packet;
        _ ->
            <<Packet/bits, 0:8>>
        end.


build_tcp_options(PropList) ->
    MSS = proplists:get_value(mss, PropList, 0),
    if MSS > 0 ->
            <<2:8, 4:8, MSS:16/integer-unsigned-big>>;
        true -> 
            <<>>
    end.


build_tcp_packet(PropList) ->
    UrgentPointer = 0,
    Options = build_tcp_options(PropList),
    TcpHeaderLen = 5 + (byte_size(Options) div 4),
    Payload = proplists:get_value(payload, PropList, <<>>),
    TcpLen = (TcpHeaderLen * 4) + byte_size(Payload),
    HalfHeader1 = <<(proplists:get_value(sport, PropList, 0)):16/integer-unsigned-big, 
                    (proplists:get_value(dport, PropList, 0)):16/integer-unsigned-big, 
                    (proplists:get_value(seq_num, PropList, 0)):32/integer-unsigned-big,
                    (proplists:get_value(ack_num, PropList, 0)):32/integer-unsigned-big,
                    TcpHeaderLen:4, 
                    (proplists:get_value(reserved, PropList, 0)):6, 
                    (proplists:get_value(urg, PropList, 0)):1, 
                    (proplists:get_value(ack, PropList, 0)):1, 
                    (proplists:get_value(psh, PropList, 0)):1, 
                    (proplists:get_value(rst, PropList, 0)):1, 
                    (proplists:get_value(syn, PropList, 0)):1, 
                    (proplists:get_value(fin, PropList, 0)):1, 
                    (proplists:get_value(win_size, PropList, 0)):16/integer-unsigned-big>>,
    HalfHeader2 = <<UrgentPointer:16/integer-unsigned-big, Options/binary, Payload/binary>>,
    PseduIpHeader = psedu_ip_tcp_header(proplists:get_value(sip, PropList, {0, 0, 0, 0}), 
                                        proplists:get_value(dip, PropList, {0, 0, 0, 0}), 
                                        TcpLen),
    TcpChecksum = checksum(psedu_tcp_packet(<<PseduIpHeader/bits, HalfHeader1/bits, 
                                               0:16, HalfHeader2/bits>>)),
    <<HalfHeader1/bits, TcpChecksum:16/integer-unsigned-big, HalfHeader2/bits>>.


build_ip_packet(PropList) ->
    {Sip1, Sip2, Sip3, Sip4} =  (proplists:get_value(sip, PropList, {0, 0, 0, 0})),
    {Dip1, Dip2, Dip3, Dip4} =  (proplists:get_value(dip, PropList, {0, 0, 0, 0})),
    Payload = proplists:get_value(payload, PropList, <<>>),
    HeaderLen = 5,
    TotalLenByte = (HeaderLen * 4) + byte_size(Payload),
    HalfHeader1 = <<(proplists:get_value(version, PropList, ?IPV4)):4,
                    HeaderLen:4,
                    (proplists:get_value(tos, PropList, 0)):8,
                    TotalLenByte:16/integer-unsigned-big,
                    (proplists:get_value(id, PropList, 0)):16/integer-unsigned-big,
                    (proplists:get_value(flags, PropList, 0)):3,
                    (proplists:get_value(frag_offset, PropList, 0)):13/integer-unsigned-big,
                    (proplists:get_value(ttl, PropList, ?TTL)):8,
                    (proplists:get_value(proto, PropList, ?PROT_TCP)):8>>,
    HalfHeader2 = <<Sip1:8, Sip2:8, Sip3:8, Sip4:8, Dip1:8, Dip2:8, Dip3:8, Dip4:8>>,
    Checksum = checksum(<<HalfHeader1/bits, 0:16, HalfHeader2/bits>>),
    <<HalfHeader1/bits, Checksum:16/integer-unsigned-big, HalfHeader2/bits, Payload/binary>>.


build_eth_packet_with_pad(SrcMAC, DstMAC, Type, Payload) ->
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


build_eth_packet(SrcMAC, DstMAC, Type, Payload) ->
    <<DstMAC:48/bits, SrcMAC:48/bits, Type:16/integer-unsigned-big, 
      Payload/bits>>.


nic_mac(NicName) ->
    [{_NicName, _Index, MAC, _HwType, _MTU}] = tables:lookup_nic(NicName),
    MAC.


decode_tcp_options(Packet, PakInfo) ->
    <<Kind:8, Rest/binary>> = Packet,
    case Kind of
        ?TCP_OPTION_END ->
            {ok, PakInfo};
        ?TCP_OPTION_NOP ->
            decode_tcp_options(Rest, PakInfo);
        ?TCP_OPTION_MSS ->
            <<_LenMss:8, Mss:16/integer-unsigned-big, Rest2/binary>> = Rest,
            decode_tcp_options(Rest2, ?PAKINFO{mss = Mss});
        _ ->
            <<Len:8, _/binary>> = Rest,
            <<_UnknowOption:Len/binary, Rest3/binary>> = Packet,
            decode_tcp_options(Rest3, PakInfo)
        end.


decode_tcp(TcpPacket, PakInfo) ->
    TcpLen = ?PAKINFO.ip_total_len - (?PAKINFO.ip_header_len * 4),
    PseduIpHeader = psedu_ip_tcp_header(?PAKINFO.sip, ?PAKINFO.dip, TcpLen),
    case checksum(psedu_tcp_packet(<<PseduIpHeader/bits, TcpPacket/bits>>)) of
        0 ->
            <<Sport:16/integer-unsigned-big, Dport:16/integer-unsigned-big, 
              SeqNum:32/integer-unsigned-big,
              AckNum:32/integer-unsigned-big,
              HeaderLenth:4, _Reserved:6, 
              _URG:1, ACK:1, _PSH:1, RST:1, SYN:1, FIN:1, WinSize:16,
              _Checksum:16/integer-unsigned-big, _UrgentPoniter:16/integer-unsigned-big,
              TcpRest/binary>> = TcpPacket,

            HeaderLenBytes = HeaderLenth * 4,
            <<_TcpHeader:HeaderLenBytes/binary, Data/binary>> = TcpPacket,
            PakInfo2 = ?PAKINFO{tcp_sport = Sport, tcp_dport = Dport, 
                                seq_num = SeqNum, ack_num = AckNum,
                                tcp_header_len = HeaderLenth, 
                                ack = ACK, rst = RST, syn = SYN, fin = FIN,
                                window_size = WinSize, tcp_packet = TcpPacket,
                                tcp_data = Data},
            case HeaderLenBytes > ?TCP_BASE_HEADER_LEN of
                true ->
                    decode_tcp_options(TcpRest, PakInfo2);
                false ->
                    {ok, PakInfo2}
                end;
        _ ->
            {error, err_tcp_checksum}
        end.


decode_icmp(_IcmpPacket, _PakInfo) ->
    {error, noicmp}.


decode_ip(IpPacket, PakInfo) ->
    <<Version:4, HeaderLen:4, Packet/binary>> = IpPacket, 
    IpHeaderLenBytes = HeaderLen * 4,
    <<IpHeader:IpHeaderLenBytes/binary, _/binary>> = IpPacket,
    case checksum(IpHeader) of
        0 ->
            case Version of
                ?IPV4 ->
                    <<_TOS:8, TotalLen:16/integer-unsigned-big,
                      _ID:16, _Flg:3, _FragOffset:13,
                      _TTL:8, Protocol:8, _Checksum:16,  
                      Sip1:8, Sip2:8, Sip3:8, Sip4:8,
                      Dip1:8, Dip2:8, Dip3:8, Dip4:8,
                      Rest/binary>> = Packet,

                    PayloadLen = TotalLen - IpHeaderLenBytes,
                    <<IpPayload:PayloadLen/binary, _/binary>> = Rest,
                    PakInfo2 = PakInfo#pkinfo{ip_version = Version, 
                                              ip_header_len = HeaderLen, 
                                              ip_total_len = TotalLen,
                                              ip_protocol = Protocol, 
                                              sip = {Sip1, Sip2, Sip3, Sip4},
                                              dip = {Dip1, Dip2, Dip3, Dip4},
                                              ip_packet = IpPacket},
                    case Protocol of 
                        ?PROT_ICMP ->
                            decode_icmp(IpPayload, PakInfo2);
                        ?PROT_TCP ->
                            decode_tcp(IpPayload, PakInfo2);
                        _ ->
                            {error, unknow_ip_prot}
                    end;
                _OtherVersion ->
                    {error, err_ip_version}
            end;
        _ ->
            {error, err_ip_checksum}
    end.


decode_arp(_ArpPacket, _PakInfo) ->
    {error, noarp}.


decode_packet(Packet) ->
    <<Dmac1:8, Dmac2:8, Dmac3:8, Dmac4:8, Dmac5:8, Dmac6:8, 
      Smac1:8, Smac2:8, Smac3:8, Smac4:8, Smac5:8, Smac6:8,
      EthType:16/integer-unsigned-big,
      EthPayload/binary>> = Packet,

    PakInfo = #pkinfo{dmac = {Dmac1, Dmac2, Dmac3, Dmac4, Dmac5, Dmac6},
                      smac = {Smac1, Smac2, Smac3, Smac4, Smac5, Smac6},
                      eth_type = EthType},

    case EthType of
        ?TYPE_ARP ->
            decode_arp(EthPayload, PakInfo);
        ?TYPE_IP ->
            decode_ip(EthPayload, PakInfo);
        _ ->
            {error, unknow_eth_type}
    end.
    
