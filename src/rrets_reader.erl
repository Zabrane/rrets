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

-module(rrets_reader).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

-export([
    open/1,
    open/2,
    match/1,
    close/1
]).

-record(continuation, {
    filename,
    process_fun,
    wl_continuation 
}).

open(Filename) ->
    {ok, Continuation} = wrap_log_reader:open(Filename),
    Fun = fun(Terms) -> Terms end,
    #continuation{filename=Filename, wl_continuation=Continuation, process_fun=Fun}.

%%
open(Filename, MatchPattern) ->
    CompiledMatchPattern = ets:match_spec_compile(MatchPattern),
    {ok, Continuation} = wrap_log_reader:open(Filename),
    Fun = fun(Terms) -> ets:match_spec_run(Terms, CompiledMatchPattern) end,
    #continuation{filename=Filename, wl_continuation=Continuation, process_fun=Fun}.

match(Continuation) ->
    chunk(Continuation).

chunk(Continuation) ->
    case process_chunk(Continuation) of
        {[], Continuation1} ->
            chunk(Continuation1);
        Result ->
            Result
    end.

close(#continuation{wl_continuation=Continuation}) ->
    wrap_log_reader:close(Continuation).

%%
%% Helpers
%% 
    
process_chunk(#continuation{wl_continuation=Continuation, process_fun=F}=C) ->
    case wrap_log_reader:chunk(Continuation) of
        {error, _Reason}=Error ->
            Error;
        {Continuation1, eof} ->
            {eof, C#continuation{wl_continuation=Continuation1}};
        {Continuation1, Terms} ->
            {F(Terms), C#continuation{wl_continuation=Continuation1}};
        {Continuation1, Terms, BadBytes} ->
            logger:info("~p bytes lost.", [BadBytes]),
            {F(Terms), C#continuation{wl_continuation=Continuation1}}
    end.

