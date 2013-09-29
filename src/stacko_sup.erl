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
    {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
    %% init tables
    tables:create_ip_table(),
    {ok, Pid}.
    

init([]) ->
    SuperSpec = {one_for_one, 5, 5},
    IpSpec = {ip_sup,                    % id
               {ip_sup, start_link, []}, % {Module, Function, Arguments}
               permanent,                % Restart
               brutal_kill,              % Shutdown
               supervisor,               % Type
               [ip_sup]},                % ModuleList
    ConfSpec = {conf,                          % id
                {conf, start_link, []},        % {Module, Function, Arguments}
                permanent,                     % Restart
                brutal_kill,                   % Shutdown
                worker,                        % Type
                [conf]},                       % ModuleList
    {ok, {SuperSpec, [IpSpec, ConfSpec]}}.
