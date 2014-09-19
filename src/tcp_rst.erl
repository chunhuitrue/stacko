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
-include("stacko.hrl").
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
    case tcp:decode_packet(Packet) of
        {error, _Reason} ->
            ok;
        {ok, PakInfo} ->
            RetTcpPak = tcp:build_tcp_pak(?PAKINFO.dip, ?PAKINFO.sip, 
                                          ?PAKINFO.tcp_dport, ?PAKINFO.tcp_sport,
                                          0, ?PAKINFO.seq_num + 1,
                                          0, 1, 0, 1, 0, 0, 
                                          0, <<>>),
            RetIpPak = tcp:build_ip_pak(?PAKINFO.dip, ?PAKINFO.sip, 0, 0, ?PROT_TCP, RetTcpPak),
            case arp:get_dst_mac2(?PAKINFO.sip) of
                {error, noroute} ->
                    ok;
                {error, _, _} ->
                    ok;
                {DstMAC, NicName, NicIndex} ->
                    SrcMAC = tcp:nic_mac(NicName),
                    RetEthPak = tcp:build_eth_pak(SrcMAC, DstMAC, ?TYPE_IP, RetIpPak),
                    nic_out:send(NicIndex, RetEthPak);
                _ ->
                    ok
            end
        end,
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
