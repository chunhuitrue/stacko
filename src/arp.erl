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


-module(arp).

-behaviour(gen_server).
-export([init/1]).
-export([start_link/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-export([to_arp/1]).
-export([acd/0]).


init([]) ->
    {ok, null}.


start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).


handle_cast({Iface, Packet}, _State) ->
    %% io:format("arp get a packet!~n"),
    Packet,
    Iface,
    {noreply, null};
handle_cast(acd, _State) ->
    gratuitous(ets:first(ip_table)),
    {noreply, null}.


handle_call(_Request, _Rrom, _State) ->
    {noreply, null}.


handle_info(_Request, _State) ->
    {noreply, null}.


terminate(_Reason, _STate) ->
    ok.


code_change(_Oldv, _State, _Extra) ->
    {ok, null}.


to_arp(Res) ->
    gen_server:cast(arp, Res).


acd() ->
    case is_pid(whereis(arp))  of 
        true ->
            gen_server:cast(arp, acd);
        _ ->
            timer:sleep(10),
            acd()
    end.


gratuitous(First) ->
    case First of
        '$end_of_table' ->
            io:format("end_of_table.~n"),
            ok;
        _ ->
            io:format("one ip.~n"),
            %% [{Ip, _Mask, Nic, Mac}] = tables:lookup_ip(First),

            %% DstMac = <<16#ff:8, 16#ff:8, 16#ff:8, 16#ff:8, 16#ff:8, 16#ff:8, 16#ff:8, 16#ff:8>>,

            %% [A, B, C, D, E, F] = Mac,
            %% SrcMac = <<16#A:8, 16#B:8, 16#C:8, 16#D:8, 16#E:8, 16#F:8>>,


            %% Packet = <<DstMac:48, SrcMac:48>>,
            gratuitous(ets:next(ip_table, First))
    end.


