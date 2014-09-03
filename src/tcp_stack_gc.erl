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

-include("head.hrl").

-behaviour(gen_server).
-export([init/1]).
-export([start_link/0]).
-export([handle_cast/2]).
-export([handle_call/3]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).



start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init([]) ->
    {ok, null, 0}.


handle_cast(_Request, _State) ->
    {noreply, _State}.


handle_call(_Request, _From, _State) ->
    {noreply, _State}.


stack_gc(Key, Pid) ->
    case catch tcp_stack:check_alive(Pid) of
        {'EXIT', {noproc, _}} ->
            ?DBP("tcp_stack_gc del pid: ~p~n", [Pid]),
            tables:del_stack(Key);
        _ ->
            ok
    end.


handle_info(timeout, _State) ->
    erlang:send_after(?STACK_GC_TIME, self(), stack_gc),
    {noreply, _State};

handle_info(stack_gc, _State) ->
    ets:foldl(fun({Key, Val}, ok) -> 
                      stack_gc(Key, Val), 
                      ok
              end,
              ok,
              tcp_stack_table),

    erlang:send_after(?STACK_GC_TIME, self(), stack_gc),
    {noreply, _State}.


terminate(_Reason, _STate) ->
    ok.


code_change(_Oldv, _State, _Extra) ->
    {ok, _State}.

