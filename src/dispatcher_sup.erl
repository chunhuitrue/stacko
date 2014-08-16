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



-module(dispatcher_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-export([start_dispatcher/1]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    {ok, {{one_for_one, 5, 5}, []}}.


start_dispatcher(MaxIndex) ->
    dispatcher(MaxIndex),
    stacko_sup:start_nic(MaxIndex).


dispatcher(MaxIndex) ->
    lists:map(fun(Spec) -> supervisor:start_child(dispatcher_sup, Spec) end, 
              [{Name,                             % id
                {dispatcher, start_link, [Name]}, % {Module, Function, Arguments}
                permanent,                        % Restart
                brutal_kill,                      % Shutdown
                worker,                           % Type
                [dispatcher]}                    % ModuleList
               || N <- lists:seq(0, MaxIndex), 
                  Name <- [list_to_atom(atom_to_list(dispatcher) ++ integer_to_list(N))]]).
    
                    
