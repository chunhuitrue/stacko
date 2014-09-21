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
-include("stacko.hrl").

-behaviour(gen_server).
-define(SERVER, ?MODULE).
-export([start_link/1,
         init/1, 
         handle_call/3, 
         handle_cast/2, 
         handle_info/2,
         terminate/2, 
         code_change/3]).

-export([query_state/1,
         check_alive/1]).

-record(state, {listenpid, userpid, userref, 
                localip, 
                localport, 
                init_seq, 
                self_mac, 
                state,
                opposite_ip, 
                opposite_port,
                opposite_mss,
                opposite_mac}).


close(Localip, Localport, RemoteIp, Remoteport) ->
    tables:del_stack(RemoteIp, Remoteport, Localip, Localport),
    gen_server:cast(tcp_port_mgr, {release_port, Localport}),
    exit(normal).


send_packet(Sip, Dip, TcpPacket) ->
    IpPacket = tcp:build_ip_packet([{sip, Sip},
                                    {dip, Dip},
                                    {payload, TcpPacket}]),
    case arp:get_dst_mac2(Dip) of
        {error, noroute} ->
            {error, noroute};
        {error, A, B} ->
            {error, A, B};
        {DstMAC, NicName, NicIndex} ->
            SrcMAC = tcp:nic_mac(NicName),
            EthPacket = tcp:build_eth_packet(SrcMAC, DstMAC, ?TYPE_IP, IpPacket),
            nic_out:send(NicIndex, EthPacket);
        _ ->
            {error, unknown}
    end.
    

recv_a_syn_packet(PakInfo, State) ->
    tables:insert_stack({?PAKINFO.sip, ?PAKINFO.tcp_sport, ?PAKINFO.dip, ?PAKINFO.tcp_dport},
                       self()),
    gen_server:cast(tcp_port_mgr, {inc_ref, ?PAKINFO.tcp_dport}),

    SynAckPacket = tcp:build_tcp_packet([{sip, ?PAKINFO.dip},
                                         {dip, ?PAKINFO.sip},
                                         {sport, ?PAKINFO.tcp_dport},
                                         {dport, ?PAKINFO.tcp_sport},
                                         {seq_num, ?STATE.init_seq},
                                         {ack_num, ?PAKINFO.seq_num + 1},
                                         {ack, 1},
                                         {syn, 1},
                                         {win_size, ?TCP_WINDOW_SIZE},
                                         {mss, ?MSS}]),
    send_packet(?PAKINFO.dip, ?PAKINFO.sip, SynAckPacket),

    ?STATE{localip = ?PAKINFO.dip,
           localport = ?PAKINFO.tcp_dport,
           opposite_ip = ?PAKINFO.sip,
           opposite_port = ?PAKINFO.tcp_sport,
           state = syn_recvd,
           opposite_mss = ?PAKINFO.mss}.


send_ack(PakInfo) ->
    AckPak = tcp:build_tcp_packet([{sip, ?PAKINFO.dip},
                                   {dip, ?PAKINFO.sip},
                                   {sport, ?PAKINFO.tcp_dport},
                                   {dport, ?PAKINFO.tcp_sport},
                                   {seq_num, 1},
                                   {ack_num, ?PAKINFO.seq_num + byte_size(?PAKINFO.tcp_data)},
                                   {ack, 1},
                                   {win_size, ?TCP_WINDOW_SIZE}]),
    send_packet(?PAKINFO.dip, ?PAKINFO.sip, AckPak).


%% remote address: Sip Sport
%% local address: Dip Dport
start_link(ListenPid) ->
    gen_server:start_link(?MODULE, [ListenPid], []).


init([ListenPid]) ->
    {ok, #state{listenpid = ListenPid}, 0}.


handle_cast({userpid, UserPid}, State) ->
    ?DBP("tcp_stack: ~p get user pid: ~p~n", [self(), UserPid]),
    Ref = erlang:monitor(process, UserPid),
    {noreply, ?STATE{userpid = UserPid,
                     userref = Ref}};


handle_cast({packet, Packet}, State) ->
    case tcp:decode_packet(Packet) of
        {error, _Reason} ->
            {noreply, State};
        {ok, PakInfo} ->
            if 
                ?PAKINFO.syn == 1, ?STATE.state == closed ->
                    io:format("recv date syn: ~p~n", [?PAKINFO.tcp_data]),
                    NewState = recv_a_syn_packet(PakInfo, State),
                    {noreply, NewState};
                ?PAKINFO.ack_num == ?STATE.init_seq + 1, ?STATE.state == syn_recvd ->
                    io:format("recv date ack: ~p~n", [?PAKINFO.tcp_data]),
                    io:format("established~n"),
                    gen_server:cast(?STATE.listenpid, {ready_accept, self()}),
                    {noreply, ?STATE{state = established}};
                byte_size(?PAKINFO.tcp_data) > 0 ->
                    send_ack(PakInfo),
                    io:format("recv date data: ~p~n", [?PAKINFO.tcp_data]),
                    timer:sleep(100),
                    {noreply, State};
               true ->
                    {noreply, State}
            end
    end.


handle_call(listen_close, From, State) ->
    gen_server:reply(From, ok),
    close(?STATE.localip, ?STATE.localport, ?STATE.opposite_ip, ?STATE.opposite_port),
    {noreply, ?STATE{state = fin_wait_1}};


handle_call({close, UserPid}, From, State) when ?STATE.userpid == UserPid ->
    gen_server:reply(From, ok),
    erlang:demonitor(?STATE.userref),
    close(?STATE.localip, ?STATE.localport, ?STATE.opposite_ip, ?STATE.opposite_port),
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
    {noreply, ?STATE{init_seq = tcp_seq:init_seq(),
                     state = closed}};


handle_info({'DOWN', _Ref, process, _Pid, _Reason}, State) ->
    ?DBP("tcp_stack: ~p. user app is donw.~n", [self()]),
    erlang:demonitor(?STATE.userref),
    close(?STATE.localip, ?STATE.localport, ?STATE.opposite_ip, ?STATE.opposite_port),
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
