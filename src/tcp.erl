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



print_listen('$end_of_table') ->
    ok;
print_listen(First) ->
    [{Port, Pid}] = tables:lookup_listen(First),
    io:format("    ~p            ~p~n", [Port, Pid]),
    print_listen(ets:next(tcp_listen_table, First)).


netstat() ->
    io:format("listening port    pid~n"),
    print_listen(ets:first(tcp_listen_table)).


listen(Port, _Options) ->
    Backlog = 3,
    case gen_server:call(tcp_port_res, {listen, Port, Backlog, self()}) of
        {ok, ListenPid} ->
            Ref = erlang:monitor(process, ListenPid),
            {ok, {Ref, ListenPid}};
        Ret ->
            Ret
    end.
    

close({Ref, ListenPid}) ->
    erlang:demonitor(Ref),
    gen_server:cast(ListenPid, {close, self()}).

