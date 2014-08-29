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



-module(tcp_stack).

-behaviour(gen_server).
-export([init/1]).
-export([start_link/4]).
-export([handle_cast/2]).
-export([handle_call/3]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {sip, sport, dip, dport}).



%% remote address: Sip Sport
%% local address: Dip Dport
start_link(Sip, Sport, Dip, Dport) ->
    gen_server:start_link(?MODULE, [Sip, Sport, Dip, Dport], []).


init([Sip, Sport, Dip, Dport]) ->
    {ok, #state{sip = Sip, sport = Sport, dip = Dip, dport = Dport}}.


handle_cast(die, State) ->
    io:format("tcp stack: ~p die~n", [self()]),
    supervisor:terminate_child(tcp_stack_sup, self()),
    {noreply, State}.


handle_call(echo, _Rrom, _State) ->
    {reply, echo, _State}.


handle_info(_Request, _State) ->
    {noreply, _State}.


terminate(_Reason, _STate) ->
    ok.


code_change(_Oldv, _State, _Extra) ->
    {ok, _State}.
