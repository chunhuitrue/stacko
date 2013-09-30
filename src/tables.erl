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


-module(tables).

-export([create_ip/0]).
-export([lookup_ip/1]).
-export([insert_ip/4]).
-export([del_ip/1]).

-export([create_arp/0]).
-export([lookup_arp/1]).
-export([insert_arp/4]).
-export([del_arp/1]).


%% ip table
%% ip mask Iface mac
create_ip() ->
    ets:new(ip_table, [set, public, named_table, public]).


lookup_ip(Ip) ->
    ets:lookup(ip_table, Ip).


insert_ip(Ip, Mask, Nic, Mac) ->
    ets:insert(ip_table, [{Ip, Mask, Nic, Mac}]).


del_ip(Ip) ->
    ets:delete(ip_table, Ip).


%% arp table
%% ip HwType Mac Nic time
create_arp() ->
    ets:new(arp_table, [set, public, named_table, public]).


lookup_arp(Ip) ->
    ets:lookup(ip_table, Ip).


insert_arp(Ip, HwType, Mac, Nic) ->
    ets:insert(arp_table, [{Ip, HwType, Mac, Nic}]).


del_arp(Ip) ->
    ets:delete(arp_table, Ip).
