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



-module(tcp_stack_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).
-export([start_child/4]).

-define(SERVER, ?MODULE).



start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    TcpStack = {tcp_stack,
                {tcp_stack, start_link, []},
                temporary,
                brutal_kill,
                worker,
                [tcp_stack]},

    {ok, {{simple_one_for_one, 0, 1}, [TcpStack]}}.


%% remote address: Sip Sport
%% local address: Dip Dport
start_child(Sip, Sport, Dip, Dport) ->
    supervisor:start_child(?SERVER, [Sip, Sport, Dip, Dport]).



