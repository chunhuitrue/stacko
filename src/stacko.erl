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


-module(stacko).

-export([nic_up/1]).
-export([nic_down/1]).
-export([arp/0]).
-export([conf_ip/3]).
-export([mac_to_binary/1]).
-export([get_ip_from_nic/1]).
-export([route/1]).
-export([route/2]).
-export([route/6]).
-export([get_num_ip/1]).

-export([test_arp/0]).


nic_up(NicName) ->
    case nif:nic_up(NicName) of
        {error, Resion} ->
            {error, Resion};
        {Index, MAC, HwType, MTU}  ->
            tables:insert_nic(NicName, Index, MAC, HwType, MTU)
    end.


nic_down(NicName) ->
    nif:nic_down(NicName),
    tables:del_nic(NicName).


arp('$end_of_table') ->
    ok;
arp(First) ->
   [{IP, HwType, MAC, NIC, _Time}] = tables:lookup_arp(First),
    io:format("~w ~w ~w ~w~n", [IP, HwType, MAC, NIC]),
    arp(ets:next(arp_table, First)).
arp() ->
   arp(ets:first(arp_table)).


test_arp() ->
    MAC = <<16#ff:8, 16#ff:8, 16#ff:8, 16#ff:8, 16#ff:8, 16#ff:8>>,
    {_MegaSecs, Now, _MicroSecs} = erlang:now(),
    tables:insert_arp({192, 168, 1, 20}, 1, MAC, p2p1, Now),
    tables:insert_arp({192, 168, 1, 21}, 1, MAC, p2p1, Now),
    tables:insert_arp({192, 168, 1, 22}, 1, MAC, p2p1, Now).


conf_ip(Ip, Mask, Nic) ->
    tables:insert_ip(Ip, Mask, Nic).


mac_to_binary(Mac) ->
    case length(Mac) of
        6 ->
            mac_to_binary(Mac, <<>>);
        _ ->
            badarg
    end.
mac_to_binary([], Acc) ->
    Acc;
mac_to_binary([A | Tail], Acc) ->
    if is_atom(A) ->
            Data2 = list_to_integer(atom_to_list(A), 16),
            mac_to_binary(Tail, list_to_binary([Acc, <<Data2:8>>]));
       is_integer(A) ->
            Data = list_to_integer(integer_to_list(A), 16),
            mac_to_binary(Tail, list_to_binary([Acc, <<Data:8>>]))
    end.


get_ip_from_nic('$end_of_table', _NicName) ->
    null;
get_ip_from_nic(First, NicName) ->
    case tables:lookup_ip(First) of
        [{IP, _Mask, NicName}] ->
            IP;
        _ ->
            get_ip_from_nic(ets:next(ip_table, First), NicName)
    end.
get_ip_from_nic(NicName) ->
    get_ip_from_nic(ets:first(ip_table), NicName).


route('$end_of_table') ->
    ok;
route(First) when is_integer(First)  ->
    Res = ets:lookup(route_table, First),
    io:format("~w~n", [Res]),
    route(ets:next(route_table, First));
route(print) ->
    route(ets:first(route_table)).


route(add, Destiantion, Gateway, Mask, Flag, NIC) ->
    case ets:last(route_table) of
        '$end_of_table' ->
            tables:insert_route(0, Destiantion, Gateway, Mask, Flag, NIC);
        LastIndex ->
            tables:insert_route(LastIndex + 1, Destiantion, Gateway, Mask, Flag, NIC)
    end.


route(del, Num) ->
    tables:del_route(Num).


get_num_ip({A, B, C, D}) ->
    Res = (A * math:pow(2, 24)) + (B * math:pow(2, 16)) + (C * math:pow(2, 8)) + D,
    round(Res).
