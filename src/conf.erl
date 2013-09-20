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


-module(conf).

-behaviour(gen_server).
-export([init/1]).
-export([start_link/0]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-define(LOOP_TIME, 1000).


init([]) ->
    erlang:send_after(?LOOP_TIME, self(), watch_conf),
    {ok, null}.


start_link() ->
    {ok, Pid} = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
    test_conf(),
    {ok, Pid}.


handle_info(watch_conf, _State) ->
    load_conf(),
    erlang:send_after(?LOOP_TIME, self(), watch_conf),
    {noreply, null}.


handle_call(_Request, _Rrom, _State) ->
    {noreply, null}.


handle_cast(_Request, _State) ->
    {noreply, null}.


terminate(_Reason, _STate) ->
    ok.


code_change(_Oldv, _State, _Extra) ->
    {ok, null}.


load_conf() ->
    %% io:format("in watch conf file!~n",[]),
    ok.

test_conf() ->
    %% 10 dispatchers
    l23_sup:start_dispatcher(10 -1).
