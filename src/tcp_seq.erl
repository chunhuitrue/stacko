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



-module(tcp_seq).

-include("head.hrl").

-behaviour(gen_server).
-define(SERVER, ?MODULE).
-export([start_link/0,
         init/1, 
         handle_call/3, 
         handle_cast/2, 
         handle_info/2,
         terminate/2, 
         code_change/3]).

-export([init_seq/0]).



init_seq() ->
    gen_server:call(?SERVER, init_seq).


start_link() -> 
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


init(Args) ->
    {ok, Args}.


handle_call(init_seq, _From, State) ->
    {reply, 1, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
