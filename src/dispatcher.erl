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

-include("head.hrl").

-behaviour(gen_server).
-export([init/1]).
-export([start_link/0]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).



start_link() ->
    gen_server:start_link(?MODULE, [], []).


init([]) ->
    {ok, null, 0}.


handle_cast({packet, Packet}, _State) ->
    <<_DMAC:48, _SMAC:48, Type:16/integer-unsigned-big, _Payload/binary>> = Packet,
    case Type  of
        ?TYPE_ARP ->                      
            arp:to_arp(Packet);
        ?TYPE_IP ->
            dispatch_ip(Packet);
        _ ->
            ok
    end,
    {noreply, _State}.


handle_call(_Request, _From, _State) ->
    {noreply, _State}.


handle_info(timeout, _State) ->
    nic_in ! {regist, self()}, 
    {noreply, _State}.


terminate(_Reason, _State) ->
    ok.


code_change(_Oldv, State, _Extra) ->
    {ok, State}.


dispatch_tcp(Packet) ->
    <<_DMAC:48, _SMAC:48, _Type:16/integer-unsigned-big, % mac header
      _Version:4, _Head:68, _Protocol:8, _HeaderCheckSum:16, % ip header
      SipD1:8, SipD2:8, SipD3:8, SipD4:8, 
      DipD1:8, DipD2:8, DipD3:8, DipD4:8, 
      _Sport:16/integer-unsigned-big, Dport:16/integer-unsigned-big, % tcp header
      _SeqNum:32,
      _AckNum:32,
      _HeaderLenth:4, _Reserved:6, _URG:1, _ACK:1, _PSH:1, _RST:1, SYN:1, _FIN:1, _WinSize:16,
      _Rest/binary>> = Packet,
    
    Sip = {SipD1, SipD2, SipD3, SipD4},
    Dip = {DipD1, DipD2, DipD3, DipD4},
    IsMyip = tables:is_my_ip(Dip),
    {PortIsListening, ListenPid} = tcp:port_is_listening(Dport),

    if IsMyip == true, PortIsListening == true, SYN == 1 ->
            tcp_listen:syn_packet(ListenPid, {syn, Packet});
       true ->
            ok
    end,

    {Sip, Dip}.


dispatch_ip(Packet) ->
    <<_DMAC:48, _SMAC:48, _Type:16/integer-unsigned-big, 
      Version:4, _Head:68, Protocol:8, _Rest/binary>> = Packet,

    if Version =:= ?IPV4, Protocol =:= ?PROT_ICMP ->
            icmp:to_icmp(Packet);
       Version =:= ?IPV4, Protocol =:= ?PROT_TCP ->
            dispatch_tcp(Packet);
       Version =:= ?IPV4, Protocol =:= ?PROT_UDP  ->
            ok;
       true ->
            ok
    end.




