
-module(rrets_test).

-include_lib("eunit/include/eunit.hrl").


open_and_log_test() ->
    Args = [
        {name, test_log},
        {file, "test_log"},
        {size, {1024, 4}}],

    {ok, R} = rrets:open(Args),

    rrets:log(R, {bami, 1}),
    rrets:log(R, {nasi, 2}),
    rrets:log(R, {soto_ajam, 2}),

    receive 
        nothing -> ok
    after 1000 ->
            ok
    end,

    rrets:log(R, {krupuk, 1}),
    rrets:log(R, {sate, 2}),
    rrets:log(R, {nasi, 3}),
    rrets:log(R, {bami_moksi, 4}),

    %% Get all nasi terms.
    C = rrets_reader:open("test_log", [{{'$1',{nasi,'$2'}},[],[{{'$1',{{nasi,'$2'}}}}]}]),
    {Records, C1} = rrets_reader:match(C),
    rrets_reader:close(C1),
    true = length(Records) > 2,

    rrets:log(R, {krupuk, 10}),

    %% Get all krupuk terms.
    C2 = rrets_reader:open("test_log", [{{'$1',{krupuk,'$2'}},[],[{{'$1',{{krupuk,'$2'}}}}]}]),
    {Records2, C3} = rrets_reader:match(C2),
    rrets_reader:close(C3),
    true = length(Records2) > 2,

    ok.

unix_time_test() ->
    T = rrets_utils:unix_time(),
    {{_Year, _Month, _Day}, {_Hour, _Min, _Sec}} = rrets_utils:unix_time_to_datetime(T),

    ok.
    

    
