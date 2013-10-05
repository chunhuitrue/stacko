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



init([]) ->
    {ok, null}.


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
    

handle_info(_Request, _State) ->
    {noreply, null}.


handle_call(_Request, _Rrom, _State) ->
    {noreply, null}.


handle_cast(Packet, _State) ->
    <<_DMAC:48, _SMAC:48, _Type:16/integer-unsigned-big,
      _Version:4, HeadLen4Byte:4, _TOS:8, TotalLenByte:16/integer-unsigned-big,
      _ID:16, _Flg:3, _FragOff:13/integer-unsigned-big,
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
            <<Type:8, Code:8, _CRC:16/integer-unsigned-big, Date/binary>> = ICMP,
            CRC = stacko:checksum(ICMP),

            if CRC =:= 0, Type =:= 8, Code =:= 0 ->
                    io:format("====================== request me! ~n");
               true ->
                    ok
            end;
        false ->
            ok
    end,

    {noreply, null}.


terminate(_Reason, _STate) ->
    ok.


code_change(_Oldv, _State, _Extra) ->
    {ok, null}.


to_icmp(Packet) ->
    gen_server:cast(icmp, Packet).
