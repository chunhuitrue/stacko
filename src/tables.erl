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

-export([create_tables/0]).

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
-export([is_my_ip/1]).

-export([create_route/0]).
-export([insert_route/6]).
-export([del_route/1]).
-export([find_route/1]).

-export([create_listen/0]).
-export([lookup_listen/1]).
-export([insert_listen/2]).
-export([del_listen/1]).

-export([create_stack/0]).
-export([lookup_stack/1]).
-export([insert_stack/2]).
-export([del_stack/1]).



create_tables() ->
    create_ip(),
    create_arp(),
    create_nic(),
    create_route(),
    create_stack().


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


is_my_ip(IP) ->
    case lookup_ip(IP) of
        [{_IP, _Mask, _Nic}] ->
            true;
        [] ->
            false
    end.


%% route table
%% num  destination  gateway  mask  flag  nic
create_route() ->
    ets:new(route_table, [ordered_set,  public, named_table, public]).
    

insert_route(Num, Destiantion, Gateway, Mask, Flag, NIC) ->
    ets:insert(route_table, [{Num, Destiantion, Gateway, Mask, Flag, NIC}]).


del_route(Num) ->
    ets:delete(route_table, Num).


find_route('$end_of_table', _IP, null) -> 
    noroute;
find_route('$end_of_table', _IP, {_DestIP, Gateway, _Mask, Flag, NIC}) -> 
    case lists:member($G, Flag) of
        true ->
            {gateway, Gateway, NIC};
        false ->
            {direct, NIC}
    end;
find_route(First, IP, Acc) ->
    [{_Num, DestIPTuple, Gateway, MaskTuple, Flag, NIC}] = ets:lookup(route_table, First),
    DestIP = stacko:get_num_ip(DestIPTuple),
    Mask = stacko:get_num_ip(MaskTuple),
    IPNum = stacko:get_num_ip(IP),

    if (IPNum band Mask) == DestIP ->
            case Acc of
                null ->
                    find_route(ets:next(route_table, First), 
                               IP, {DestIPTuple, Gateway, MaskTuple, Flag, NIC});
                {_DestIPAcc, _GatewayAcc, MaskAccTuple, _FlagAcc, _NICAcc} ->
                    MaskAcc = stacko:get_num_ip(MaskAccTuple),
                    if Mask > MaskAcc ->
                            find_route(ets:next(route_table, First), 
                                       IP, {DestIPTuple, Gateway, MaskTuple, Flag, NIC});
                       Mask =< MaskAcc ->
                            find_route(ets:next(route_table, First), IP, Acc)
                    end
            end;
       (IPNum band Mask) /= DestIP ->
            find_route(ets:next(route_table, First), IP, Acc)
    end.
find_route(IP) ->
    find_route(ets:first(route_table), IP, null).


%% tcp_listen table
%% key: Port 
%% val: Pid
create_listen() ->
    ets:new(tcp_listen_table, [set, public, named_table, public]).


lookup_listen('$end_of_table', _Pid) ->
    [];
lookup_listen(First, Pid) ->
    case lookup_listen(First) of
        [{Port, Pid}] ->
            Port;  
        [] ->
            lookup_listen(ets:next(tcp_listen_table, First), Pid)
    end.


lookup_listen(Pid) when is_pid(Pid) ->
    lookup_listen(ets:first(tcp_listen_table), Pid);
lookup_listen(Port) when is_integer(Port) ->
    ets:lookup(tcp_listen_table, Port).


insert_listen(Port, Pid) ->
    ets:insert(tcp_listen_table, [{Port, Pid}]).


del_listen(Pid) when is_pid(Pid) ->
    ets:delete(tcp_listen_table, lookup_listen(Pid));
del_listen(Port) when is_integer(Port) ->
    ets:delete(tcp_listen_table, Port).


%% tcp_stack table
%% key: {Sip, Sport, Dip, Dport}
%% val: Pid
%% local address: dip, dport 
create_stack() ->
    ets:new(tcp_stack_table, [set, public, named_table, public]).


lookup_stack({Sip, Sport, Dip, Dport}) ->
    ets:lookup(tcp_stack_table, {Sip, Sport, Dip, Dport}).


insert_stack({Sip, Sport, Dip, Dport}, Pid) ->
    ets:insert(tcp_stack_table, [{{Sip, Sport, Dip, Dport}, Pid}]).


del_stack({Sip, Sport, Dip, Dport}) ->
    ets:delete(tcp_stack_table, {Sip, Sport, Dip, Dport}).
