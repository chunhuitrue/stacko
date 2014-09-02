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

-record(state, {port, backlogarg, backlog, userpid, stackmap, queue, wait_accepts}).



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
            stackmap = maps:new(),
            queue = [],
            wait_accepts = []}, 
     0}.


handle_cast({syn, _Packet}, State) when State#state.backlog >= State#state.backlogarg ->
    io:format("tcp_listen: get a syn packet. but backlog is full.~n"),
    {noreply, State};

handle_cast({syn, Packet}, State) ->
    %% io:format("tcp_listen: get a syn packet.~n"),
    {ok, StackPid} = tcp_stack_sup:start_child(self()),
    StackRef = erlang:monitor(process, StackPid),
    tcp_stack:to_tcp_stack({syn, Packet}, StackPid),
    {noreply, State#state{backlog = State#state.backlog + 1, 
                          stackmap = maps:put(StackPid, StackRef, State#state.stackmap)}};

handle_cast({ready_accept, StackPid}, State) when State#state.wait_accepts == [] ->
    io:format("tcp_listen: stack: ~p ready to be accept, wait_accepts is empty. add to queue: ~p~n", [StackPid, State#state.queue]),
    {noreply, State#state{queue = [StackPid | State#state.queue]}};

handle_cast({ready_accept, StackPid}, State) ->
    io:format("tcp_listen: stack: ~p ready to be accept, wait_accepts is not empty. ~n", 
              [StackPid]),
    StackRef = maps:get(StackPid, State#state.stackmap),
    erlang:demonitor(StackRef),
    Tmp = lists:reverse(State#state.wait_accepts),
    AcceptRef = erlang:hd(Tmp),
    gen_server:reply(AcceptRef, {ok, StackPid}),
    {noreply, State#state{backlog = State#state.backlog - 1,
                          stackmap = maps:remove(StackPid, State#state.stackmap),
                          wait_accepts = lists:reverse(erlang:tl(Tmp))}}.


close() ->
    supervisor:terminate_child(tcp_listen_sup, self()).


handle_call({accept, _AcceptPid}, From, State) when State#state.queue == [] ->
    io:format("tcp_listen: no link to accept.~n"),
    {noreply, State#state{wait_accepts = [From | State#state.wait_accepts]}};

handle_call({accept, _AcceptPid}, _From, State) ->
    TmpQueue = lists:reverse(State#state.queue),
    StackPid = erlang:hd(TmpQueue),
    StackRef = maps:get(StackPid, State#state.stackmap),
    erlang:demonitor(StackRef),
    {reply, 
     {ok, StackPid}, 
     State#state{backlog = State#state.backlog - 1,
                 stackmap = maps:remove(StackPid, State#state.stackmap),
                 queue = lists:reverse(erlang:tl(TmpQueue))}};

handle_call({close, _UserPid}, From, State) ->
    gen_server:reply(From, ok),
    close(),
    {noreply, State}.


remove_pid(Pid, List) ->
    lists:filter(fun(Elem) -> Elem =/= Pid end, List).


handle_info({'DOWN', _UserRef, process, UserPid, _Reason},
            State) when UserPid == State#state.userpid ->  % user listen process is down
    io:format("tcp_listen: user: ~p is down. I'm going to die.~n", [UserPid]),
    close(),
    {noreply, State};

handle_info({'DOWN', _Ref, process, StackPid, _Reason}, State) -> % tcp stack process down
    io:format("tcp_listen: tcp stack process: ~p is down. so backlog - 1.~n", [StackPid]),
    {noreply, 
     State#state{backlog = State#state.backlog - 1,
                 stackmap = maps:remove(StackPid, State#state.stackmap),
                 queue = remove_pid(StackPid, State#state.queue)}};

handle_info(timeout, State) ->
    erlang:monitor(process, State#state.userpid),
    io:format("tcp_listen: init timeout~n"),
    {noreply, State}.


terminate(_Reason, _STate) ->
    ok.


code_change(_Oldv, State, _Extra) ->
    {ok, State}.


syn_packet(Pid, {syn, Packet}) ->
    gen_server:cast(Pid, {syn, Packet}).
