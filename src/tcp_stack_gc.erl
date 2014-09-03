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


-module(tcp_stack_gc).

-behaviour(gen_server).
-export([init/1]).
-export([start_link/0]).
-export([handle_cast/2]).
-export([handle_call/3]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-export([test/1]).



start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init([]) ->
    {ok, null}.


handle_cast(_Request, _State) ->
    {noreply, _State}.


handle_call(_Request, _From, _State) ->
    {noreply, _State}.


handle_info(_Request, _State) ->
    {noreply, _State}.


terminate(_Reason, _STate) ->
    ok.


code_change(_Oldv, _State, _Extra) ->
    {ok, _State}.


test(Pid) ->
    case catch gen_server:call(Pid, echo) of
        {'EXIT', {noproc, _}} ->
            io:format("Pid: ~p must dead. ~n", [Pid]);
        echo ->
            io:format("Pid: ~p is alive.~n", [Pid])
    end.
