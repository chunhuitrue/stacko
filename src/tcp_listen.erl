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

-include("head.hrl").

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
    ?DBP("tcp_listen start. port: ~p, backlog: ~p, userpid: ~p~n", 
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


handle_cast({syn, _Packet}, State) when ?STATE.backlog >= ?STATE.backlogarg ->
    ?DBP("tcp_listen: get a syn packet. but backlog is full.~n"),
    {noreply, State};

handle_cast({syn, Packet}, State) ->
    ?DBP("tcp_listen: get a syn packet.~n"),
    {ok, StackPid} = tcp_stack_sup:start_child(self()),
    StackRef = erlang:monitor(process, StackPid),
    gen_server:cast(StackPid, {packet, Packet}),
    {noreply, ?STATE{backlog = ?STATE.backlog + 1, 
                     stackmap = maps:put(StackPid, StackRef, ?STATE.stackmap)}};

handle_cast({ready_accept, StackPid}, State) when ?STATE.wait_accepts == [] ->
    ?DBP("tcp_listen: stack: ~p ready to be accept, wait_accepts is empty. add to queue: ~p~n", [StackPid, ?STATE.queue]),
    {noreply, ?STATE{queue = [StackPid | ?STATE.queue]}};

handle_cast({ready_accept, StackPid}, State) ->
    ?DBP("tcp_listen: stack: ~p ready to be accept, wait_accepts is not empty. ~n", 
              [StackPid]),
    StackRef = maps:get(StackPid, ?STATE.stackmap),
    erlang:demonitor(StackRef),
    Tmp = lists:reverse(?STATE.wait_accepts),
    AcceptRef = erlang:hd(Tmp),
    gen_server:reply(AcceptRef, {ok, StackPid}),
    {noreply, ?STATE{backlog = ?STATE.backlog - 1,
                     stackmap = maps:remove(StackPid, ?STATE.stackmap),
                     wait_accepts = lists:reverse(erlang:tl(Tmp))}}.


close(State) ->
    tables:del_listen(?STATE.port),
    gen_server:cast(tcp_port_mgr, {release_port, ?STATE.port}),
    lists:map(fun(StackPid) -> gen_server:call(StackPid, listen_close) end, ?STATE.queue),
    supervisor:terminate_child(tcp_listen_sup, self()).


handle_call(accept, From, State) when ?STATE.queue == [] ->
    ?DBP("tcp_listen: no link to accept.~n"),
    {noreply, ?STATE{wait_accepts = [From | ?STATE.wait_accepts]}};

handle_call(accept, _From, State) ->
    TmpQueue = lists:reverse(?STATE.queue),
    StackPid = erlang:hd(TmpQueue),
    StackRef = maps:get(StackPid, ?STATE.stackmap),
    erlang:demonitor(StackRef),
    {reply, {ok, StackPid}, ?STATE{backlog = ?STATE.backlog - 1,
                                   stackmap = maps:remove(StackPid, ?STATE.stackmap),
                                   queue = lists:reverse(erlang:tl(TmpQueue))}};

handle_call({close, _UserPid}, From, State) ->
    gen_server:reply(From, ok),
    close(State),
    {noreply, State}.


remove_pid(Pid, List) ->
    lists:filter(fun(Elem) -> Elem =/= Pid end, List).


handle_info({'DOWN', _UserRef, process, UserPid, _Reason},
            State) when UserPid == ?STATE.userpid ->  % user listen process is down
    ?DBP("tcp_listen: user: ~p is down. I'm going to die.~n", [UserPid]),
    close(State),
    {noreply, State};

handle_info({'DOWN', _Ref, process, StackPid, _Reason}, State) -> % tcp stack process down
    ?DBP("tcp_listen: tcp stack process: ~p is down. so backlog - 1.~n", [StackPid]),
    {noreply, ?STATE{backlog = ?STATE.backlog - 1,
                     stackmap = maps:remove(StackPid, ?STATE.stackmap),
                     queue = remove_pid(StackPid, ?STATE.queue)}};

handle_info(timeout, State) ->
    erlang:monitor(process, ?STATE.userpid),
    gen_server:cast(tcp_monitor, {monitor_me, self()}),
    tables:insert_listen(?STATE.port, self()),
    {noreply, State}.


terminate(_Reason, _STate) ->
    ok.


code_change(_Oldv, State, _Extra) ->
    {ok, State}.


syn_packet(Pid, {syn, Packet}) ->
    gen_server:cast(Pid, {syn, Packet}).
