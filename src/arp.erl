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

-behaviour(gen_server).
-export([init/1]).
-export([start_link/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-export([to_arp/1]).


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
    <<_DstMAC:48/bits, SrcMAC:48/bits, 
      _Type:16, 
      HwType:16/integer-unsigned-big, ProtType:16/integer-unsigned-big,
      HardSize:8/integer-unsigned-big, ProtSize:8/integer-unsigned-big,
      Op:16/integer-unsigned-big,
      SenderMAC:48/bits, SenderIP:32/bits, TargetMAC:48/bits, TargetIP:32/bits,
      _Rest/binary>> = Packet,

    case Op of
        1 ->                                    
            <<A:8, B:8, C:8, D:8>> = TargetIP,
            RequestIP = {A, B, C, D},
            case tables:lookup_ip(RequestIP) of
                [{RequestIP, _Mask, Nic}] ->
                    io:format("arp get a packet. it is me. ~w~n", [RequestIP]),
                    RePacket = <<>>;
                [] ->
                    io:format("arp get a packet. it is not me. ~w~n", [RequestIP])
            end;
        2 ->
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
