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



-module(stacko_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-export([start_nic/1]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).
    

init([]) ->
    DispatcherSpec = {dispatcher_sup,                   % id
                      {dispatcher_sup, start_link, []}, % {Module, Function, Arguments}
                      permanent,                        % Restart
                      brutal_kill,                      % Shutdown
                      supervisor,                       % Type
                      [dispatcher_sup]},                % ModuleList

    IcmpSpec = {icmp,                  % id
               {icmp, start_link, []}, % {Module, Function, Arguments}
               permanent,              % Restart
               brutal_kill,            % Shutdown
               worker,                 % Type
               [icmp]},                % ModuleList

    ArpSpec = {arp,                      % id
               {arp, start_link, [arp]}, % {Module, Function, Arguments}
               permanent,                % Restart
               brutal_kill,              % Shutdown
               worker,                   % Type
               [arp]},                   % ModuleList

    TcpListenSpec = {tcp_listen,             
                     {tcp_listen, start_link, []},
                     permanent,  
                     brutal_kill,       
                     worker,
                     [tcp_listen]}, 

    TcpSpec = {tcp_sup,             
               {tcp_sup, start_link, []},
               permanent,         
               brutal_kill,       
               supervisor,        
               [tcp_sup]}, 

    {ok, {{one_for_one, 5, 5}, [DispatcherSpec, ArpSpec, IcmpSpec, TcpListenSpec, TcpSpec]}}.


start_nic(DispatcherNum) ->
    ReadSpec = {nicread_gen,                              % id
                {nic_in, start_link, [DispatcherNum]},    % {Module, Function, Arguments}
                permanent,                                % Restart
                brutal_kill,                              % Shutdown
                worker,                                   % Type
                [nic_in]},                                % ModuleList
    supervisor:start_child(stacko_sup, ReadSpec),

    WriteSpec = {nicwrite,                          % id
                 {nic_out, start_link, [nicwrite]}, % {Module, Function, Arguments}
                 permanent,                         % Restart
                 brutal_kill,                       % Shutdown
                 worker,                            % Type
                 [nic_out]},                        % ModuleList
    supervisor:start_child(stacko_sup, WriteSpec).




