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


-module(tables).

-export([create_ip_table/0]).
-export([lookup_ip_table/1]).
-export([insert_ip_table/3]).
-export([del_ip_table/1]).


%% ip table
create_ip_table() ->
    ets:new(ip_table, [set, public, named_table, public]).


lookup_ip_table(Ip) ->
    ets:lookup(ip_table, Ip).


insert_ip_table(Ip, Mask, Nic) ->
    ets:insert(ip_table, [{Ip, Mask, Nic}]).


del_ip_table(Ip) ->
    ets:delete(ip_table, Ip).
