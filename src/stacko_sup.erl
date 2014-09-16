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



start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).
    

init([]) ->
    {ok, 
     {{one_for_one, 5, 5}, 
      [
       {nic_out,                           % id
        {nic_out, start_link, []},         % {Module, Function, Arguments}
        permanent,                         % Restart
        brutal_kill,                       % Shutdown
        worker,                            % Type
        [nic_out]},                        % ModuleList

       {icmp,                  % id
        {icmp, start_link, []}, % {Module, Function, Arguments}
        permanent,              % Restart
        brutal_kill,            % Shutdown
        worker,                 % Type
        [icmp]},                % ModuleList

       {arp,                      % id
        {arp, start_link, [arp]}, % {Module, Function, Arguments}
        permanent,                % Restart
        brutal_kill,              % Shutdown
        worker,                   % Type
        [arp]},                   % ModuleList

       {dispatcher_sup,                   % id
        {dispatcher_sup, start_link, []}, % {Module, Function, Arguments}
        permanent,                        % Restart
        brutal_kill,                      % Shutdown
        supervisor,                       % Type
        [dispatcher_sup]},                % ModuleList

       {tcp_monitor,
        {tcp_monitor, start_link, []},
        permanent,
        brutal_kill,
        worker,
        [tcp_monitor]},

       {init_seq,
        {init_seq, start_link, []},
        permanent,
        brutal_kill,
        worker,
        [init_seq]},

       {tcp_port_mgr,
        {tcp_port_mgr, start_link, []},
        permanent,
        brutal_kill,
        worker,
        [tcp_port_mgr]},

       {tcp_listen_sup,
        {tcp_listen_sup, start_link, []},
        permanent,
        2000,
        supervisor,
        [tcp_listen_sup]},

       {tcp_stack_sup,
        {tcp_stack_sup, start_link, []},
        permanent,
        2000,
        supervisor,
        [tcp_stack_sup]}
]}}.


