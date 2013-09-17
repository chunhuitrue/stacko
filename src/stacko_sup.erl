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
-export([start_in_shell/0]).


start_in_shell() ->
    {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
    unlink(Pid),
    conf_file(),
    {ok, Pid}.

start_link() ->
    {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
    %% conf_file(),
    {ok, Pid}.

init([]) ->
    SuperSpec = {one_for_one, 5, 5},
    %% l23 sup
    ChildSpec = {l23_sup,                       % id
                 {l23_sup, start_link, []},     % {Module, Function, Arguments}
                 permanent,                     % Restart
                 1000,                          % Shutdown
                 supervisor,                    % Type
                 [l23_sup]},                    % ModuleList
    %% todo l4 sup
    {ok, {SuperSpec, [ChildSpec]}}.

conf_file() ->
    l23_sup:start_agent_in(),
    l23_sup:start_agent_out().
