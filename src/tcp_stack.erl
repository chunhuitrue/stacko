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



-module(tcp_stack).

-behaviour(gen_server).
-export([init/1]).
-export([start_link/1]).
-export([handle_cast/2]).
-export([handle_call/3]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-export([to_tcp_stack/2]).

-record(state, {listenpid}).



%% remote address: Sip Sport
%% local address: Dip Dport
start_link(ListenPid) ->
    gen_server:start_link(?MODULE, [ListenPid], []).


init([ListenPid]) ->
    {ok, #state{listenpid = ListenPid}}.


ready_accept(ListenPid) ->
    gen_server:cast(ListenPid, {ready_accept, self()}).

    
handle_cast({syn, Packet}, State) ->
    io:format("tcp_stack: 11111.~n"),

    <<_DMAC:48, _SMAC:48, _Type:16/integer-unsigned-big, % mac header
      _Version:4, _Head:68, _Protocol:8, _HeaderCheckSum:16, % ip header
      SipD1:8, SipD2:8, SipD3:8, SipD4:8, 
      DipD1:8, DipD2:8, DipD3:8, DipD4:8, 
      Sport:16/integer-unsigned-big, Dport:16/integer-unsigned-big, % tcp header
      _SeqNum:32,
      _AckNum:32,
      _HeaderLenth:4, _Reserved:6, _URG:1, _ACK:1, _PSH:1, _RST:1, _SYN:1, _FIN:1, _WinSize:16,
      _Rest/binary>> = Packet,
    
    Sip = {SipD1, SipD2, SipD3, SipD4},
    Dip = {DipD1, DipD2, DipD3, DipD4},

    tables:insert_stack({Sip, Sport, Dip, Dport}, self()),
    io:format("tcp_stack: get a syn packet. insert_stack: Sip: ~p, Sport: ~p, Pid: ~p~n", [Sip, Sport, self()]),
    io:format("Dip: ~p, Dport: ~p~n", [Dip, Dport]),



    %% timer:sleep(6000),
    %% 2 = 3,

    ready_accept(State#state.listenpid),
    


    {noreply, State}.


handle_call(_Request, _Rrom, State) ->
    {noreply, State}.


handle_info(_Request, State) ->
    {noreply, State}.


terminate(_Reason, _STate) ->
    ok.


code_change(_Oldv, _State, _Extra) ->
    {ok, _State}.


to_tcp_stack({syn, Packet}, Pid) ->
    gen_server:cast(Pid, {syn, Packet}).
