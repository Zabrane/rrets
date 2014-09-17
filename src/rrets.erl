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

-module(rrets).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

%% calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}).
-define(UNIX_EPOCH, 62167219200).

-export([
    open/1,
    log/2,
    info/1,
    sync/1,
    match_open/2,
    match/1,
    match_close/1,
    close/1
]).

-record(continuation, {
        filename,
        match_spec,
        wrap_log_continuation 
}).


open(Args) ->
    %%   {name, test_log},
    %%   {file, "test_log"},
    %%   {size, {1024, 4}},
    DiskLogArgs = [{type, wrap}, {format, internal} | Args],

    case disk_log:open(DiskLogArgs) of
        {error, _Reason}=Error ->
            lager:error("Error triggered while opening. Error: ~p.", [Error]),
            Error;
        {ok, Log} -> 
            {ok, Log};
        {repaired, Log, {recovered, Rec}, {badbytes, Bad}} ->
            lager:info("Repair triggered while opening ~s. "
                "Info: ~p terms recovered, ~p bytes lost.", [Log, Rec, Bad]),
            {ok, Log}
    end.

log(Log, Term) ->
    Entry = {unix_time(), Term},
    ok = disk_log:alog(Log, Entry),
    ok.

info(Log) ->
    disk_log:info(Log).

sync(Log) ->
    disk_log:sync(Log).

close(Log) ->
    ok = disk_log:close(Log),
    ok.


%%
match_open(Filename, MatchPattern) ->
    CompiledMatchPattern = ets:match_spec_compile(MatchPattern),
    {ok, Continuation} = wrap_log_reader:open(Filename),
    #continuation{filename=Filename, wrap_log_continuation=Continuation, match_spec=CompiledMatchPattern}.

match(#continuation{wrap_log_continuation=Continuation, match_spec=MatchSpec}=C) ->
    case wrap_log_reader:chunk(Continuation) of
        {error, _Reason}=Error ->
            Error;
        {Continuation1, eof} ->
            {eof, C#continuation{wrap_log_continuation=Continuation1}};
        {Continuation1, Terms} ->
            {ets:match_spec_run(Terms, MatchSpec), C#continuation{wrap_log_continuation=Continuation1}};
        {Continuation1, Terms, BadBytes} ->
            lager:info("~p bytes lost.", [BadBytes]),
            {ets:match_spec_run(Terms, MatchSpec), C#continuation{wrap_log_continuation=Continuation1}}
    end.

match_close(#continuation{wrap_log_continuation=Continuation}) ->
    wrap_log_reader:close(Continuation).
    

%%
%% Helpers
%%

unix_time() ->
    datetime_to_unix_time(erlang:universaltime()).

datetime_to_unix_time({{_,_,_},{_,_,_}}=DateTime) ->
    calendar:datetime_to_gregorian_seconds(DateTime) - ?UNIX_EPOCH.

