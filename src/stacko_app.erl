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



-module(stacko_app).

-include("head.hrl").

-behaviour(application).
-export([start/2, stop/1]).


start(_StartType, _StartArgs) ->
    tables:create_tables(),
    {ok, Pid} = stacko_sup:start_link(),
    stacko:init_script(),
    {ok, Pid}.


stop(_State) ->
    stacko:release_conf().

