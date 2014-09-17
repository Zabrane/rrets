
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

    rrets:close(R),

    %% Get all nasi records.
    C = rrets:match_open("test_log", [{{'$1',{nasi,'$2'}},[],[{{'$1',{{nasi,'$2'}}}}]}]),
    io:fwrite(standard_error, "Match: ~p~n", [rrets:match(C)]),
    rrets:match_close(C),

    C1 = rrets:match_open("test_log", [{{'$1',{krupuk,'$2'}},[],[{{'$1',{{krupuk,'$2'}}}}]}]),
    io:fwrite(standard_error, "Match: ~p~n", [rrets:match(C1)]),
    rrets:match_close(C1),

    ok.

    
