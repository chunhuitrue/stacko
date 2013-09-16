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

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Superspec = {one_for_one, 10, 3600},
    %% {Id, {Module, Function, Arguments}, Restart, Shutdown, Type, ModuleList}
    Childspec = {l23_sup,                       % id 和 l23 中 start_link 的名字冲突？
                 {l23_sup, start_link, []},     % {Module, Function, Arguments}
                 permanent,                     % Restart
                 1000,                          % Shutdown
                 supervisor,                    % Type
                 l23_sup},                      % ModuleList
    {ok, {Superspec, [Childspec]}}.
    %% {ok, { {one_for_one, 5, 10}, []} }.

