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


-module(stacko).

-export([mac_to_binary/1]).


mac_to_binary(Mac) ->
    mac_to_binary(Mac, <<>>).
mac_to_binary([], Acc) ->
    Acc;
mac_to_binary([A | Tail], Acc) ->
    if is_atom(A) ->
            Data2 = list_to_integer(atom_to_list(A), 16),
            mac_to_binary(Tail, list_to_binary([Acc, <<Data2:8>>]));
       is_integer(A) ->
            Data = list_to_integer(integer_to_list(A), 16),
            mac_to_binary(Tail, list_to_binary([Acc, <<Data:8>>]))
    end.




