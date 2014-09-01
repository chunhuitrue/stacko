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

-export([syn_packet/2]).

-record(state, {port, backlogarg, backlog, userpid, userref, map, queue}).



start_link(Port, Backlog, UserPid) ->
    io:format("tcp_listen start. port: ~p, backlog: ~p, userpid: ~p~n", 
              [Port, Backlog, UserPid]),
    gen_server:start_link(?MODULE, [Port, Backlog, UserPid], []).


init([Port, Backlog, UserPid]) ->
    {ok,
     #state{port = Port, 
            backlogarg = Backlog, 
            backlog = 0, 
            userpid = UserPid, 
            userref = null,
            map = maps:new(),
            queue = []}, 
     0}.


close(UserRef) ->
    erlang:demonitor(UserRef),
    supervisor:terminate_child(tcp_listen_sup, self()).


handle_cast({close, _UserPid}, State) ->
    #state{userref = UserRef} = State,
    close(UserRef),
    {noreply, State};


handle_cast({syn, _Packet}, State) when State#state.backlog >= State#state.backlogarg ->
    io:format("tcp_listen: get a syn packet. but backlog is full.~n"),
    {noreply, State};

handle_cast({syn, Packet}, State) ->
    %% io:format("tcp_listen: get a syn packet.~n"),
    {ok, StackPid} = tcp_stack_sup:start_child(self()),
    StackRef = erlang:monitor(process, StackPid),
    tcp_stack:to_tcp_stack({syn, Packet}, StackPid),
    {noreply, State#state{backlog = State#state.backlog + 1, 
                          map = maps:put(StackPid, StackRef, State#state.map)}};


handle_cast({ready_accept, Pid}, State) ->
    io:format("tcp_listen: stack: ~p established, add to queue: ~p~n", 
              [Pid, State#state.queue]),
    {noreply, State#state{queue = [Pid | State#state.queue]}}.


handle_call(_Request, _From, State) ->
    {noreply, State}.


remove_pid(Pid, List) ->
    lists:filter(fun(Elem) -> Elem =/= Pid end, List).


handle_info({'DOWN', UserRef, process, UserPid, _Reason}, % user listen process is down
            State) when UserPid == State#state.userpid ->
    io:format("tcp_listen: user: ~p is down. I'm going to die.~n", [UserPid]),
    close(UserRef),
    {noreply, State};

handle_info({'DOWN', _Ref, process, StackPid, _Reason}, State) -> % tcp stack process down
    io:format("tcp_listen: tcp stack process: ~p is down. so backlog - 1.~n", [StackPid]),
    {noreply, 
     State#state{backlog = State#state.backlog - 1,
                 map = maps:remove(StackPid, State#state.map),
                 queue = remove_pid(StackPid, State#state.queue)}};


handle_info(timeout, State) ->
    UserRef = erlang:monitor(process, State#state.userpid),
    io:format("tcp_listen: init timeout.userref: ~p~n", [UserRef]),
    {noreply, State#state{userref = UserRef}}.


terminate(_Reason, _STate) ->
    ok.


code_change(_Oldv, State, _Extra) ->
    {ok, State}.


syn_packet(Pid, {syn, Packet}) ->
    gen_server:cast(Pid, {syn, Packet}).
