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


-module(tcp_listen).

-behaviour(gen_server).
-export([init/1]).
-export([start_link/3]).
-export([handle_cast/2]).
-export([handle_call/3]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {port, backlog, userpid, userref}).



start_link(Port, Backlog, UserPid) ->
    io:format("tcp_listen start. port: ~p, backlog: ~p, userpid: ~p~n", 
              [Port, Backlog, UserPid]),
    gen_server:start_link(?MODULE, [Port, Backlog, UserPid], []).


init([Port, Backlog, UserPid]) ->
    {ok, #state{port = Port, backlog = Backlog, userpid = UserPid, userref = null}, 0}.


close(UserRef) ->
    erlang:demonitor(UserRef),
    supervisor:terminate_child(tcp_listen_sup, self()).


handle_cast({close, _UserPid}, State) ->
    #state{userref = UserRef} = State,
    close(UserRef),
    {noreply, State}.


handle_call(_Request, _From, State) ->
    {noreply, State}.


handle_info({'DOWN', UserRef, process, _Pid, _Reason}, State) ->
    close(UserRef),
    {noreply, State};

handle_info(timeout, State) ->
    UserRef = erlang:monitor(process, State#state.userpid),
    io:format("tcp_listen timeout.userref: ~p~n", [UserRef]),
    {noreply, State#state{userref = UserRef}}.


terminate(_Reason, _STate) ->
    ok.


code_change(_Oldv, State, _Extra) ->
    {ok, State}.


