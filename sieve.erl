%%% @author John Daily <jd@epep.us>
%%% @copyright (C) 2012, John Daily
%%% @doc
%%%   Assignment from programmingpraxis.com: Sieve of Eratosthenes
%%%
%%% @reference See <a href="http://programmingpraxis.com/2009/02/19/sieve-of-eratosthenes/">Sieve of Eratosthenes</a>.
%%% @end
%%% Created : 20 Oct 2012 by John Daily <jd@epep.us>

-module(sieve).
-export([sieve/1, sieve/2, start_timing/2, run_sieve/2]).

start_timing(Upper, HowMany) ->
    show_report(Upper, HowMany,
                [ { serial, average_time(fun sieve/1, [ Upper ], HowMany, HowMany, 0) },
                  { 'parallel/2', average_time(fun sieve/2, [ Upper, 2 ], HowMany, HowMany, 0) },
                  { 'parallel/3', average_time(fun sieve/2, [ Upper, 3 ], HowMany, HowMany, 0) },
                  { 'parallel/5', average_time(fun sieve/2, [ Upper, 5 ], HowMany, HowMany, 0) }
                ]
               ).

report_line([]) ->
    done;
report_line([ { Which, Average} | T]) ->
    io:format("  ~15s  :  ~5.B seconds~n", [ Which, Average ]),
    report_line(T).

show_report(Upper, Qty, Results) ->
    io:format("Results for primes/~p, ~p runs~n", [ Upper, Qty ]),
    report_line(Results).

average_time(_F, _Args, 0, OrigQty, Tally) ->
    trunc((Tally / OrigQty) / 1000000);
average_time(F, Args, Qty, OrigQty, Tally) ->
    io:format(" ... ~p/~p ...~n", [ F, Args ]),
    { Microseconds, _Return } = timer:tc(F, Args),
    io:format(" ... ~p ...~n", [ Microseconds/1000000 ]),
    average_time(F, Args, Qty - 1, OrigQty, Tally + Microseconds).

%% Our client will use this to assemble the results.
show_results(0, Results) ->
    { length(Results), Results };
show_results(NumProcs, Accum) ->
    receive
        { results, _Lower, _Upper, PrimeList } ->
            %% io:format("Received ~B from ~B:~B~n",
            %%           [ length(PrimeList), Lower, Upper ]),
            show_results(NumProcs - 1, PrimeList ++ Accum)
    %% after Delay ->
    %%         timeout
    end.


%%
%% When called with 2 arguments, uses the 2nd argument as a partition
%% count and switches to parallel mode
-spec sieve(non_neg_integer(), non_neg_integer()) -> list(non_neg_integer()).
sieve(Upper, Partitions) ->
    PartitionSize = trunc(Upper / Partitions),
    Pids = spawn_sieves(Upper, 0, Partitions, PartitionSize, []),
    AllInts = generate_int_list(2, Upper, []),
    tell_pids(Pids, Pids, PartitionSize, Upper, AllInts),
    show_results(Partitions, []).

tell_pids(PidsKeep, _Pids, _Size, Upper, [H | _T]) when H * H > Upper ->
    tell_pids_done(PidsKeep);
tell_pids(PidsKeep, [], Size, Upper, [_H | IntTail]) ->
    tell_pids(PidsKeep, PidsKeep, Size, Upper, IntTail);
tell_pids(PidsKeep, [{ Pid, Start, End } | PidTail], Size, Upper, [H | IntTail]) ->
    FirstToRemoveForThisPid = calculate_remove_start(H, Start, End),
    Pid ! { eliminate, FirstToRemoveForThisPid, H },
    tell_pids(PidsKeep, PidTail, Size, Upper, [H | IntTail]).

tell_pids_done([{ Pid, _Start, _End } | Pids]) ->
    Pid ! { self(), done },
    tell_pids_done(Pids);
tell_pids_done([]) ->
    done.

greater_of(A, B) when A > B ->
    A;
greater_of(_A, B) ->
    B.

calculate_remove_start(Value, _Start, End) when Value * Value >= End ->
    skip;
calculate_remove_start(Value, Start, _End) when Start rem Value =:= 0 ->
    greater_of(Start, Value * Value) - Start;
calculate_remove_start(Value, Start, _End) ->
    greater_of(Value * Value, Value - (Start rem Value) + Start) - Start.

%% The base case, we want to make sure the final Pid knows to take not
%% only PerPartition, but also any remainder left over by the integer
%% division.
spawn_sieves(N, Which, 1, PerPartition, Pids) ->
    Start = Which * PerPartition,
    End = Start + (N - Which * PerPartition),
    Pid = spawn(?MODULE, run_sieve, [ Start, End ]),
    [{ Pid, Start, End } | Pids];
spawn_sieves(N, Which, Partitions, PerPartition, Pids) ->
    Start = Which * PerPartition,
    End = Start + PerPartition - 1,
    Pid = spawn(?MODULE, run_sieve, [ Start, End ]),
    spawn_sieves(N, Which + 1, Partitions - 1, PerPartition, [ { Pid, Start, End } | Pids]).

run_sieve(Lower, Upper) ->
    Array = array:from_list(generate_int_list(Lower, Upper, [])),
    run_sieve_loop(Array, Lower, Upper).

run_sieve_loop(Array, Lower, Upper) ->
    receive
        { eliminate, Start, Add } when Start =< Upper ->
%%            io:format("{ ~B, ~B } received ~B + ~B elimination~n", [ Lower, Upper, Start, Add ]),
            run_sieve_loop(wipe_values(Array, Add, Start, Upper), Lower, Upper);
        { eliminate, _Start, _Add } ->
            %% If we're asked to start outside our bounds, ignore
            run_sieve_loop(Array, Lower, Upper);
        { Pid, done } ->
            Pid ! { results, Lower, Upper, pos_results(array:to_list(Array), []) };
        { Pid, report } ->
            Pid ! array:to_list(Array),
            run_sieve_loop(Array, Lower, Upper);
        Other ->
            io:format("~p~n", [ Other ])
    end.


%% Rest is the serial computation

-spec sieve(non_neg_integer()) -> list(non_neg_integer()).
sieve(N) ->
    Results = pos_results(array:to_list(filter_sieve(to_array(generate_int_list(2, N, [])), 2, N)), []),
    { length(Results), Results }.

pos_results([], Accum) ->
    lists:reverse(Accum);
pos_results([0 | T], Accum) ->
    pos_results(T, Accum);
%% XXX: Don't know where undefined values are coming from, need to sort that out.
pos_results([undefined | T], Accum) ->
    pos_results(T, Accum);
pos_results([H | T], Accum) ->
    pos_results(T, [H | Accum]).


to_array(List) ->
    array:from_list([0, 1 | List]).

-spec generate_int_list(non_neg_integer(), non_neg_integer(), list(non_neg_integer())) -> list(non_neg_integer()).
generate_int_list(Upper, Upper, List) ->
    lists:reverse([Upper | List]);
generate_int_list(Current, Upper, List) ->
    generate_int_list(Current + 1, Upper, [ Current | List ]).

filter_sieve(Array, Current, Upper) when Current * Current > Upper ->
    Array;
filter_sieve(Array, Current, Upper) ->
    filter_sieve(wipe_values(Array, Current, Current + Current, Upper),
                 Current + 1, Upper).

wipe_values(Array, _Base, Next, Upper) when Next > Upper ->
    Array;
wipe_values(Array, Base, Next, Upper) ->
%%    io:format("Remove ~B~n", [ Next ]),
    wipe_values(array:set(Next, 0, Array), Base, Next + Base, Upper).



%% -spec find_primes(non_neg_integer(), list(non_neg_integer())) -> list(non_neg_integer()).
%% find_primes(0, Primes) ->
%%     lists:reverse(Primes);
%% find_primes(N, Primes) ->
%%     find_primes(N - 1, append_if_prime(N, Primes)).

%% -spec filter_sieve(list(non_neg_integer()), list(non_neg_integer())) -> list(non_neg_integer()).
%% filter_sieve(Primes, _Filtered) ->
%%     Primes.
