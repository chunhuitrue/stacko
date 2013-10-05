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

-export([create_nic/0]).
-export([lookup_nic/1]).
-export([insert_nic/5]).
-export([del_nic/1]).

-export([create_arp/0]).
-export([lookup_arp/1]).
-export([insert_arp/5]).
-export([del_arp/1]).

-export([create_ip/0]).
-export([lookup_ip/1]).
-export([insert_ip/3]).
-export([del_ip/1]).

-export([create_route/0]).
-export([insert_route/6]).
-export([del_route/1]).



%% nic tabe
%% name, Index, Mac, HwType, MTU
create_nic() ->
    ets:new(nic_table, [set, public, named_table, public]).
    

lookup_nic(Name) ->
    ets:lookup(nic_table, Name).


insert_nic(Name, Index, MAC, HwType, MTU) ->
    ets:insert(nic_table, [{Name, Index, MAC, HwType, MTU}]).


del_nic(Name) ->
    ets:delete(nic_table, Name).


%% arp table
%% ip HwType Mac Nic time
create_arp() ->
    ets:new(arp_table, [set, public, named_table, public]).


lookup_arp(IP) ->
    ets:lookup(arp_table, IP).


insert_arp(IP, HwType, MAC, NIC, Time) ->
    ets:insert(arp_table, [{IP, HwType, MAC, NIC, Time}]).


del_arp(IP) ->
    ets:delete(arp_table, IP).


%% ip table
%% ip mask nic
create_ip() ->
    ets:new(ip_table, [set, public, named_table, public]).


lookup_ip(IP) ->
    ets:lookup(ip_table, IP).


insert_ip(IP, Mask, Nic) ->
    ets:insert(ip_table, [{IP, Mask, Nic}]).


del_ip(IP) ->
    ets:delete(ip_table, IP).


%% route table
%% index destination  gateway  mask  flag  nic
create_route() ->
    ets:new(route_table, [ordered_set,  public, named_table, public]).
    

insert_route(Index, Destiantion, Gateway, Mask, Flag, NIC) ->
    ets:insert(route_table, [{Index, Destiantion, Gateway, Mask, Flag, NIC}]).


del_route(Num) ->
    ets:delete(route_table, Num).
