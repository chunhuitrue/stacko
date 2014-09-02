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



-module(tcp).

-export([netstat/0]).

-export([listen/2]).
-export([close/1]).
-export([accept/1]).

-export([port_is_listening/1]).



print_listen('$end_of_table') ->
    ok;
print_listen(First) ->
    [{Port, Pid}] = tables:lookup_listen(First),
    io:format("~p        *:~p                *:*              LISTEN~n", [Pid, Port]),
    print_listen(ets:next(tcp_listen_table, First)).


netstat() ->
    io:format("pid             local address       foreign address     state~n"),
    print_listen(ets:first(tcp_listen_table)).


listen(Port, _Options) ->
    Backlog = 3,
    case catch gen_server:call(tcp_port_res, {listen, Port, Backlog, self()}) of
        {'EXIT', {noproc, _}} ->
            {error, noproc};
        Ret ->
            Ret
    end.
    

close(Socket) ->
    gen_server:call(Socket, {close, self()}, infinity).


accept(ListenSocket) ->
    case catch gen_server:call(ListenSocket, {accept, self()}, infinity) of
        {'EXIT', {noproc, _}} ->
            {error, closed};
        {ok, StackPid} ->
            gen_server:cast(StackPid, {userpid, self()});
        Ret ->
            Ret
    end.


port_is_listening(Port) ->
    case tables:lookup_listen(Port) of
        [] ->
            {false, null};
        [{Port, Pid}] ->
            {true, Pid}
    end.
