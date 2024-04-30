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

-module(rrets).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

%% calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}).
-define(UNIX_EPOCH, 62167219200).

-export([
    open/1,
    log/2,
    info/1,
    sync/1,
    close/1
]).

open(Args) ->
    %%   {name, test_log},
    %%   {file, "test_log"},
    %%   {size, {1024, 4}},
    DiskLogArgs = [{type, wrap}, {format, internal} | Args],

    case disk_log:open(DiskLogArgs) of
        {error, _Reason}=Error ->
            logger:error("Error triggered while opening. Error: ~p.", [Error]),
            Error;
        {ok, Log} -> 
            {ok, Log};
        {repaired, Log, {recovered, Rec}, {badbytes, Bad}} ->
            logger:info("Repair triggered while opening ~s. "
                "Info: ~p terms recovered, ~p bytes lost.", [Log, Rec, Bad]),
            {ok, Log}
    end.

log(Log, Term) ->
    Ts = rrets_utils:unix_time(),
    ok = disk_log:alog(Log, {Ts, Term}),
    Ts.

info(Log) ->
    disk_log:info(Log).

sync(Log) ->
    disk_log:sync(Log).

close(Log) ->
    ok = disk_log:close(Log).

