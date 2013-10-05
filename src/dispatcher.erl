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


-module(dispatcher).

-include("head.hrl").

-behaviour(gen_server).
-export([init/1]).
-export([start_link/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-export([to_dispatcher/2]).


init([Name]) ->
    {ok, Name}.


start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name], []).


handle_cast(Packet, StateName) ->
    <<_DMAC:48, _SMAC:48, Type:16/integer-unsigned-big, Data/binary>> = Packet,
    case Type  of
        ?TYPE_ARP ->                      
            arp:to_arp(Packet);
        ?TYPE_IP ->
            <<Version:4, _Head:68, Protocol:8, _Rest/binary>> = Data,
            case Version of
                ?IPV4 ->
                    case Protocol of
                        ?PROT_ICMP ->
                            icmp:to_icmp(Packet);
                        ?PROT_TCP ->
                            ok;
                        _ ->
                            ok
                    end;
                _ ->
                    ok
            end;
        _ ->
            ok
    end,
    {noreply, StateName}.


handle_call(_Request, _Rrom, StateName) ->
    {noreply, StateName}.


handle_info(_Request, StateName) ->
    {noreply, StateName}.


terminate(_Reason, _STate) ->
    ok.


code_change(_Oldv, StateName, _Extra) ->
    {ok, StateName}.


to_dispatcher(DispName, Packet) ->
    gen_server:cast(DispName, Packet).
