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

-include("head.hrl").

-behaviour(gen_server).
-export([init/1]).
-export([start_link/1]).
-export([handle_cast/2]).
-export([handle_call/3]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-export([query_state/1]).
-export([check_alive/1]).

-record(state, {listenpid, userpid, userref, localip, localport, remoteip, remoteport, state}).



%% remote address: Sip Sport
%% local address: Dip Dport
start_link(ListenPid) ->
    gen_server:start_link(?MODULE, [ListenPid], []).


init([ListenPid]) ->
    {ok, 
     #state{listenpid = ListenPid, 
            userpid = null,
            userref = null,
            localip = null,
            localport = null,
            remoteip = null,
            remoteport = null,
            state = closed},
    0}.


ready_accept(ListenPid) ->
    gen_server:cast(ListenPid, {ready_accept, self()}).


handle_cast({userpid, UserPid}, State) ->
    ?DBP("tcp_stack: ~p get user pid: ~p~n", [self(), UserPid]),
    Ref = erlang:monitor(process, UserPid),
    {noreply, ?STATE{userpid = UserPid,
                          userref = Ref}};

handle_cast({packet, Packet}, State) ->
    <<_DMAC:48, _SMAC:48, _Type:16/integer-unsigned-big, % mac header
      _Version:4, _Head:68, _Protocol:8, _HeaderCheckSum:16, % ip header
      SipD1:8, SipD2:8, SipD3:8, SipD4:8, 
      DipD1:8, DipD2:8, DipD3:8, DipD4:8, 
      Sport:16/integer-unsigned-big, Dport:16/integer-unsigned-big, % tcp header
      _SeqNum:32,
      _AckNum:32,
      _HeaderLenth:4, _Reserved:6, _URG:1, _ACK:1, _PSH:1, _RST:1, SYN:1, _FIN:1, _WinSize:16,
      _Rest/binary>> = Packet,
    
    Sip = {SipD1, SipD2, SipD3, SipD4},
    Dip = {DipD1, DipD2, DipD3, DipD4},

    if SYN == 1, ?STATE.localip == null ->
            tables:insert_stack({Sip, Sport, Dip, Dport}, self()),
            gen_server:cast(tcp_port_mgr, {inc_ref, Dport}),
            ?DBP("tcp_stack: ~p get a syn packet. insert_stack: Sip: ~p, Sport: ~p, Dip: ~p, Dport: ~p~n", [self(), Sip, Sport, Dip, Dport]),

            ready_accept(?STATE.listenpid),

            %% timer:sleep(5000),
            %% 2 = 3,

            {noreply, ?STATE{localip = Dip,
                             localport = Dport,
                             remoteip = Sip,
                             remoteport = Sport,
                             state = syn_recv}};
       true ->
            ?DBP("tcp_stack: ~p recv a duplicate syn packet~n", [self()]),
            {noreply, State}
    end.


close(Localip, Localport, Remoteip, Remoteport) ->
    tables:del_stack(Remoteip, Remoteport, Localip, Localport),
    tables:release_tcp_port(Localport),
    supervisor:terminate_child(tcp_stack_sup, self()).


handle_call(listen_close, From, State) ->
    gen_server:reply(From, ok),
    close(?STATE.localip, ?STATE.localport, ?STATE.remoteip, ?STATE.remoteport),
    {noreply, ?STATE{state = fin_wait_1}};

handle_call({close, UserPid}, From, State) when ?STATE.userpid == UserPid ->
    gen_server:reply(From, ok),
    erlang:demonitor(?STATE.userref),
    close(?STATE.localip, ?STATE.localport, ?STATE.remoteip, ?STATE.remoteport),
    {noreply, ?STATE{userpid = null,
                     userref = null,
                     state = fin_wait_1}};

handle_call({close, UserPid}, _From, State) when ?STATE.userpid =/= UserPid ->
    {reply, {error, permission_denied}, State};

handle_call(query_state, _From, State) ->
    {reply, {state, ?STATE.state}, State}.


handle_info(timeout, State) ->
    ?DBP("tcp_stack: start. ann tcp_monitor to monitor me~n"),
    gen_server:cast(tcp_monitor, {monitor_me, self()}),
    {noreply, State};

handle_info({'DOWN', _Ref, process, _Pid, _Reason}, State) ->
    ?DBP("tcp_stack: ~p. user app is donw.~n", [self()]),
    erlang:demonitor(?STATE.userref),
    close(?STATE.localip, ?STATE.localport, ?STATE.remoteip, ?STATE.remoteport),
    {noreply, ?STATE{userpid = null,
                     userref = null,
                     state = fin_wait_1}}.


terminate(_Reason, _STate) ->
    ok.


code_change(_Oldv, _State, _Extra) ->
    {ok, _State}.


query_state(Pid) ->
    gen_server:call(Pid, query_state).


check_alive(Pid) ->
    gen_server:call(Pid, check_alive).
