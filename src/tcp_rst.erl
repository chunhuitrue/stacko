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


-module(tcp_rst).
-include("head.hrl").
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-export([start_link/0, 
         init/1, 
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2, 
         code_change/3]).


start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


init(Args) ->
    {ok, Args}.


handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast({syn_no_listen, Packet}, State) ->
    <<_DMAC:48, _SMAC:48, _Type:16/integer-unsigned-big,      % mac head
      _Version:4, HeadLen:4, _TOS:8, TotalLen:16/integer-unsigned-big,   % ip head
      _ID:16, _Flg:3, _FragOff:13,
      _TTL:8, _Protocol:8, _CRCIP:16,  
      Sip1:8, Sip2:8, Sip3:8, Sip4:8,
      Dip1:8, Dip2:8, Dip3:8, Dip4:8,
      IpPayload/binary>> = Packet,

    Sip = {Sip1, Sip2, Sip3, Sip4},
    Dip = {Dip1, Dip2, Dip3, Dip4}, 
    
    TcpLen = (TotalLen * 8) - (HeadLen * 32),
    <<Tcp:TcpLen/bits, _Rest/binary>> = IpPayload,
    <<Sport:16/integer-unsigned-big, Dport:16/integer-unsigned-big, 
      SeqNum:32/integer-unsigned-big,
      _AckNum:32/integer-unsigned-big,
      HeaderLenth:4, _Reserved:6, _URG:1, _ACK:1, _PSH:1, _RST:1, _SYN:1, _FIN:1, _WinSize:16,
      _TcpPayload/binary>> = Tcp,
    
    io:format("tcp_rst: headerlen: ~p~n", [HeaderLenth]),

    PseduHeader = <<Dip1:8, Dip2:8, Dip3:8, Dip4:8,
                    Sip1:8, Sip2:8, Sip3:8, Sip4:8,
                    0:8, ?PROT_TCP:8, 20:16/integer-unsigned-big>>,
    RetTcpCRC = stacko:checksum(<<PseduHeader/binary,
                                  Dport:16/integer-unsigned-big, Sport:16/integer-unsigned-big,
                                  0:32/integer-unsigned-big,
                                  (SeqNum + 1):32/integer-unsigned-big,
                                  5:4, 0:6, 0:1, 1:1, 0:1, 1:1, 0:1, 0:1, 0:16,
                                  0:16, 0:16>>),
    RetTcpPak = <<Dport:16/integer-unsigned-big, Sport:16/integer-unsigned-big,
               0:32/integer-unsigned-big,
               (SeqNum + 1):32/integer-unsigned-big,
               5:4, 0:6, 0:1, 1:1, 0:1, 1:1, 0:1, 0:1, 0:16,
               RetTcpCRC:16/integer-unsigned-big, 0:16>>,
    
    RetIpPak = tcp:build_ip_pak(Dip, Sip, 0, 0, ?PROT_TCP, RetTcpPak),

    case arp:get_dst_mac2(Sip) of
        {error, noroute} ->
            io:format("tcp_rst: 111~n");
        {error, A, B} ->
            io:format("tcp_rst: Dip:~p A:~p B:~p~n", [Dip, A, B]);
        {DstMAC, NicName, NicIndex} ->
            SrcMAC = tcp:nic_mac(NicName),
            ?DBP("tcp_rst: srcmac: ~p, dstmac: ~p~n", [SrcMAC, DstMAC]),
            RetEthPak = tcp:build_eth_pak(SrcMAC, DstMAC, ?TYPE_IP, RetIpPak),
            nic_out:send(NicIndex, RetEthPak);
        _ ->
            ok
    end,
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

