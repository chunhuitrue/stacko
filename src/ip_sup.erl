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



-module(ip_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-export([start_dispatcher/1]).
-export([start_nic/1]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    ArpSpec = {arp,                      % id
               {arp, start_link, [arp]}, % {Module, Function, Arguments}
               permanent,                % Restart
               brutal_kill,              % Shutdown
               worker,                   % Type
               [arp]},                   % ModuleList
    SuperSpec = {one_for_one, 5, 5},
    {ok, {SuperSpec, [ArpSpec]}}.


start_dispatcher(N) ->
    dispatcher(N),
    start_nic(N).


dispatcher(N) ->
    if N >= 0 ->
            Name = list_to_atom(atom_to_list(dispatcher) ++ integer_to_list(N)),
            ChildSpec = {Name,                             % id
                         {dispatcher, start_link, [Name]}, % {Module, Function, Arguments}
                         permanent,                        % Restart
                         brutal_kill,                      % Shutdown
                         worker,                           % Type
                         [dispatcher]},                    % ModuleList
            supervisor:start_child(ip_sup, ChildSpec),
            dispatcher(N - 1);
       N < 0 ->
            ok
    end.


start_nic(DispatcherNum) ->
    ReadSpec = {nicread_gen,                                           % id
                {nic_in, start_link, [nicread_gen, DispatcherNum]},    % {Module, Function, Arguments}
                permanent,                                             % Restart
                brutal_kill,                                           % Shutdown
                worker,                                                % Type
                [nic_in]},                                             % ModuleList
    supervisor:start_child(ip_sup, ReadSpec),

    WriteSpec = {nicwrite,                          % id
                 {nic_out, start_link, [nicwrite]}, % {Module, Function, Arguments}
                 permanent,                         % Restart
                 brutal_kill,                       % Shutdown
                 worker,                            % Type
                 [nic_out]},                        % ModuleList
    supervisor:start_child(ip_sup, WriteSpec).




