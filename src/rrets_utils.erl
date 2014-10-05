%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2014 Maas-Maarten Zeeman
%%
%% @doc rrets, a round robin erlang term storage.
%%
%% Copyright 2014 Maas-Maarten Zeeman
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%% Erlang diff-match-patch implementation

-module(rrets_utils).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

%% calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}).
-define(UNIX_EPOCH, 62167219200).

-export([
    unix_time/0,
    unix_time/1,
    unix_time_to_datetime/1
]).

unix_time() ->
    unix_time(erlang:universaltime()).

unix_time({{_,_,_},{_,_,_}}=DateTime) ->
    datetime_to_epoch_seconds(DateTime, ?UNIX_EPOCH).

unix_time_to_datetime(Ts) ->
    epoch_seconds_to_datetime(Ts, ?UNIX_EPOCH).

datetime_to_epoch_seconds({{_,_,_},{_,_,_}}=DateTime, Epoch) ->
    calendar:datetime_to_gregorian_seconds(DateTime) - Epoch.

epoch_seconds_to_datetime(Ts, Epoch) ->
    calendar:gregorian_seconds_to_datetime(Ts + Epoch).

