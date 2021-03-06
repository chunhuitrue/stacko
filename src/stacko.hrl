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


-record(pkinfo, {dmac, smac, eth_type,                    % eth header
                 ip_version, ip_header_len, ip_total_len, % ip header
                 ip_protocol, sip, dip, ip_packet,
                 tcp_sport, tcp_dport,                    % tcp header
                 seq_num, ack_num,
                 tcp_header_len, ack, rst, syn, fin, 
                 window_size, mss, tcp_packet, tcp_data}).


-define(DISPATCHER_NUM, 3).
-define(STACK_GC_TIME, 10000).

-define(MINI_ETH_FRAME, 60).   % 64 - FCS(4)
-define(TYPE_ARP, 16#0806).
-define(TYPE_IP, 16#0800).
-define(HW_TYPE_MAC, 1).
-define(PROT_TYPE_IP, 16#0800).
-define(MAC_SIZE, 6).
-define(IP_SIZE, 4).
-define(OP_ARP_REQUEST, 1).
-define(OP_ARP_REPLAY, 2).
-define(ARP_TIMEOUT, 300000).
-define(ARP_TIMEOUT_SECOND, 300). 
-define(ARP_REFRESH_TIME, 20000).

-define(IPV4, 4).
-define(TTL, 64).

-define(PROT_UDP, 17).
-define(PROT_ICMP, 1).
-define(PROT_TCP, 6).
-define(TCP_BASE_HEADER_LEN, 20).
-define(TCP_OPTION_END, 0).
-define(TCP_OPTION_NOP, 1).
-define(TCP_OPTION_MSS, 2).
-define(TCP_WINDOW_SIZE, 4096).
-define(MSS, 1460).

-define(DEFAULT_BACKLOG, 3).

-define(STATE, State#state).
-define(PAKINFO, PakInfo#pkinfo).

-ifdef(debug).
-define(DBP(Str), io:format(Str)).
-define(DBP(Str, Args), io:format(Str, Args)).
-else.
-define(DBP(Str), ok).
-define(DBP(Str, Args), ok).
-endif.

