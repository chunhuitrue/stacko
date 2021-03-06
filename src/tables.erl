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
-export([find_listen_port/1]).

-export([create_stack/0]).
-export([lookup_stack/4]).
-export([insert_stack/2]).
-export([del_stack/4]).
-export([del_stack/1]).
-export([find_stack/1]).

-export([create_tcp_port/0]).
-export([lookup_tcp_port/1]).
-export([insert_tcp_port/2]).
-export([del_tcp_port/1]).
-export([assign_tcp_port/0]).
-export([assign_tcp_port/1]).
-export([release_tcp_port/1]).
-export([inc_port_ref/1]).



create_tables() ->
    create_ip(),
    create_arp(),
    create_nic(),
    create_route(),
    create_listen(),
    create_stack(),
    create_tcp_port().


%% nic tabe
%% name, Index, Mac, HwType, MTU
create_nic() ->
    ets:new(stacko_nic, [set, named_table, public]).
    

lookup_nic(Name) ->
    ets:lookup(stacko_nic, Name).


insert_nic(Name, Index, MAC, HwType, MTU) ->
    ets:insert(stacko_nic, [{Name, Index, MAC, HwType, MTU}]).


del_nic(Name) ->
    ets:delete(stacko_nic, Name).


%% arp table
%% ip HwType Mac Nic time
create_arp() ->
    ets:new(stacko_arp, [set, named_table, public]).


lookup_arp(IP) ->
    ets:lookup(stacko_arp, IP).


insert_arp(IP, HwType, MAC, NIC, Time) ->
    ets:insert(stacko_arp, [{IP, HwType, MAC, NIC, Time}]).


del_arp(IP) ->
    ets:delete(stacko_arp, IP).


%% ip table
%% ip mask nic
create_ip() ->
    ets:new(stacko_ip, [set, named_table, public]).


lookup_ip(IP) ->
    ets:lookup(stacko_ip, IP).


insert_ip(IP, Mask, Nic) ->
    ets:insert(stacko_ip, [{IP, Mask, Nic}]).


del_ip(IP) ->
    ets:delete(stacko_ip, IP).


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
    ets:new(stacko_route, [ordered_set, named_table, public]).
    

insert_route(Num, Destiantion, Gateway, Mask, Flag, NIC) ->
    ets:insert(stacko_route, [{Num, Destiantion, Gateway, Mask, Flag, NIC}]).


del_route(Num) ->
    ets:delete(stacko_route, Num).


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
    [{_Num, DestIPTuple, Gateway, MaskTuple, Flag, NIC}] = ets:lookup(stacko_route, First),
    DestIP = stacko:get_num_ip(DestIPTuple),
    Mask = stacko:get_num_ip(MaskTuple),
    IPNum = stacko:get_num_ip(IP),

    if (IPNum band Mask) == DestIP ->
            case Acc of
                null ->
                    find_route(ets:next(stacko_route, First), 
                               IP, {DestIPTuple, Gateway, MaskTuple, Flag, NIC});
                {_DestIPAcc, _GatewayAcc, MaskAccTuple, _FlagAcc, _NICAcc} ->
                    MaskAcc = stacko:get_num_ip(MaskAccTuple),
                    if Mask > MaskAcc ->
                            find_route(ets:next(stacko_route, First), 
                                       IP, {DestIPTuple, Gateway, MaskTuple, Flag, NIC});
                       Mask =< MaskAcc ->
                            find_route(ets:next(stacko_route, First), IP, Acc)
                    end
            end;
       (IPNum band Mask) /= DestIP ->
            find_route(ets:next(stacko_route, First), IP, Acc)
    end.
find_route(IP) ->
    find_route(ets:first(stacko_route), IP, null).


%% tcp_listen table
%% key: Port 
%% val: Pid
create_listen() ->
    ets:new(stacko_tcp_listen, [set, named_table, public]).


lookup_listen('$end_of_table', _Pid) ->
    [];
lookup_listen(First, Pid) ->
    case lookup_listen(First) of
        [{Port, Pid}] ->
            Port;  
        [] ->
            lookup_listen(ets:next(stacko_tcp_listen, First), Pid)
    end.


lookup_listen(Pid) when is_pid(Pid) ->
    lookup_listen(ets:first(stacko_tcp_listen), Pid);
lookup_listen(Port) when is_integer(Port) ->
    ets:lookup(stacko_tcp_listen, Port).


insert_listen(Port, Pid) ->
    ets:insert(stacko_tcp_listen, [{Port, Pid}]).


del_listen(Pid) when is_pid(Pid) ->
    ets:delete(stacko_tcp_listen, lookup_listen(Pid));
del_listen(Port) when is_integer(Port) ->
    ets:delete(stacko_tcp_listen, Port).


find_listen_port(Pid) when is_pid(Pid) ->
    case ets:match_object(stacko_tcp_listen, {'$1', Pid}) of
        [{Port, Pid}] ->
            Port;
        [] ->
            null
    end.
                

%% tcp_stack table
%% key: {Sip, Sport, Dip, Dport}
%% val: Pid
%% local address: dip, dport 
create_stack() ->
    ets:new(stacko_tcp_stack, [set, named_table, public]).


lookup_stack(Sip, Sport, Dip, Dport) ->
    ets:lookup(stacko_tcp_stack, {Sip, Sport, Dip, Dport}).


insert_stack({Sip, Sport, Dip, Dport}, Pid) ->
    ets:insert(stacko_tcp_stack, [{{Sip, Sport, Dip, Dport}, Pid}]).


del_stack(RemoteIP, RemotePort, LocalIP, LocalPort) ->
    ets:delete(stacko_tcp_stack, {RemoteIP, RemotePort, LocalIP, LocalPort}).


del_stack(Tuple4) when is_tuple(Tuple4) ->
    ets:delete(stacko_tcp_stack, Tuple4).


find_stack(Pid) ->
    ets:match_object(stacko_tcp_stack, {'$1', Pid}).


%% tcp port
%% key: port
%% val: RefNumber
create_tcp_port() ->
    Ret = ets:new(stacko_tcp_port, [ordered_set, named_table, public]),
    [insert_tcp_port(Port, 0) || Port <- lists:seq(1, 65535)],
    Ret.


lookup_tcp_port(Port) ->
    ets:lookup(stacko_tcp_port, Port).


insert_tcp_port(Port, RefNum) ->
    ets:insert(stacko_tcp_port, [{Port, RefNum}]).


del_tcp_port(Port) ->
    ets:delete(stacko_tcp_port, Port).


find_port('$end_of_table') ->
    {error, no_port};
find_port(First) ->
    case ets:lookup(stacko_tcp_port, First) of
        [{First, RefNum}] when RefNum == 0 ->
            {ok, First, RefNum};
        _ ->
            find_port(ets:next(stacko_tcp_port, First))
    end.


assign_tcp_port() ->
    case find_port(ets:first(stacko_tcp_port)) of
        {ok, Port, RefNum} ->
            ets:insert(stacko_tcp_port, [{Port, RefNum + 1}]),
            {ok, Port};
        Ret ->
            Ret
    end.


assign_tcp_port(Port) ->
    case ets:lookup(stacko_tcp_port, Port) of
        [{Port, RefNum}] when RefNum == 0 ->
            ets:insert(stacko_tcp_port, [{Port, 1}]),
            {ok, Port};
        _ ->
            {error, port_assigned}
    end.


release_tcp_port(Port) ->
    case ets:lookup(stacko_tcp_port, Port) of
        [{Port, RefNum}] when RefNum > 0 ->
            ets:insert(stacko_tcp_port, [{Port, RefNum - 1}]),
            ok;
        _Ret ->
            {error, repeat_release}
    end.


inc_port_ref(Port) ->
    case ets:lookup(stacko_tcp_port, Port) of
        [{Port, RefNum}] ->
            ets:insert(stacko_tcp_port, [{Port, RefNum + 1}]);
        Ret ->
            Ret
    end.
