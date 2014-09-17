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

-export([listen/2]).
-export([accept/1]).
-export([close/1]).

-export([port_is_listening/1,
         checksum/1,
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
