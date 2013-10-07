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



-module(icmp).

-include("head.hrl").

-behaviour(gen_server).
-export([init/1]).
-export([start_link/0]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-export([to_icmp/1]).
-export([ping_pid/1]).



init([]) ->
    {ok, {0, null}}.


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
    

handle_info(_Request, State) ->
    {noreply, State}.


handle_call(_Request, _Rrom, State) ->
    {noreply, State}.


handle_cast(PingPID, {IPID, _}) when is_pid(PingPID) ->
    {noreply, {IPID, PingPID}};
handle_cast(Packet, {IPID, PingPID}) ->
    <<DMAC:48/bits, _SMAC:48/bits, _Type:16,
      _Version:4, HeadLen4Byte:4, _TOS:8, TotalLenByte:16/integer-unsigned-big,
      _ID:16, _Flg:3, _FragOff:13,
      _TTL:8, _Protocol:8, _CRCIP:16,  
      SrcIP1:8, SrcIP2:8, SrcIP3:8, SrcIP4:8, 
      DstIP1:8, DstIP2:8, DstIP3:8, DstIP4:8, 
      IPPayload/binary>> = Packet,

    DstIPTuple = {DstIP1, DstIP2, DstIP3, DstIP4},
    SrcIPTuple = {SrcIP1, SrcIP2, SrcIP3, SrcIP4},
    HeadLen = HeadLen4Byte * 32,
    TotalLen = TotalLenByte * 8,
    ICMPLen = TotalLen - HeadLen,

    case tables:is_my_ip(DstIPTuple) of
        true ->    
            <<ICMP:ICMPLen/bits, _Rest/binary>> = IPPayload,
            <<Type:8, Code:8, _CRC:16, ICMPMsg/binary>> = ICMP,
            CRC = stacko:checksum(ICMP),

            if CRC =:= 0, Type =:= 8, Code =:= 0 ->
                    RetCRC = stacko:checksum(<<0:8/integer-unsigned-big, 0:8/integer-unsigned-big, 
                                               0:16/integer-unsigned-big,
                                               ICMPMsg/bits>>),
                    RetICMP = <<0:8/integer-unsigned-big, 0:8/integer-unsigned-big, 
                                RetCRC:16/integer-unsigned-big,
                                ICMPMsg/bits>>,
                    RetIPPacket = stacko:make_ip_icmp_replay(DstIPTuple, SrcIPTuple, IPID, RetICMP),

                    case arp:get_dst_mac(SrcIPTuple) of
                        {error, _} ->
                            ok;
                        {DstMAC, NicIndex} ->
                            RetEthPacket = stacko:make_eth_packet(DMAC, DstMAC, ?TYPE_IP, RetIPPacket),
                            nic_out:send(NicIndex, RetEthPacket)
                   end;
               CRC =:= 0, Type =:= 0, Code =:= 0 ->
                    to_ping(PingPID, Packet);
               true ->
                    ok
            end;
        false ->
            ok
    end,
    {noreply, {stacko:cyc_inc_32(IPID), PingPID}}.


terminate(_Reason, _State) ->
    ok.


code_change(_Oldv, State, _Extra) ->
    {ok, State}.


to_icmp(Packet) ->
    gen_server:cast(icmp, Packet).


ping_pid(Pid) ->
    gen_server:cast(icmp, Pid).


to_ping(PingPID, Packet) ->
    PingPID ! {pang, Packet}.
