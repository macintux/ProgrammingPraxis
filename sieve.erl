%%% @author John Daily <jd@epep.us>
%%% @copyright (C) 2012, John Daily
%%% @doc
%%%   Assignment from programmingpraxis.com: Sieve of Eratosthenes
%%%
%%% @reference See <a href="http://programmingpraxis.com/2009/02/19/sieve-of-eratosthenes/">Sieve of Eratosthenes</a>.
%%% @end
%%% Created : 20 Oct 2012 by John Daily <jd@epep.us>

-module(sieve).
%% -export([sieve/1]).
-compile(export_all).

-spec sieve(non_neg_integer()) -> list(non_neg_integer()).
sieve(N) ->
    pos_results(array:to_list(filter_sieve(to_array(generate_int_list(2, N, [])), 2, N)), []).

pos_results([], Accum) ->
    lists:reverse(Accum);
pos_results([0 | T], Accum) ->
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

%%-spec filter_sieve(list(non_neg_integer()), non_neg_integer()) -> list(non_neg_integer()).

%% filter_sieve(Head, [H | T], Upper) when H * H > Upper ->
%%     lists:reverse(Head) ++ [H | T];
%% filter_sieve(Head, [H | T], Upper) ->
%%     filter_sieve([H | Head], strip(H, T, []), Upper).

filter_sieve(Array, Current, Upper) when Current * Current > Upper ->
    Array;
filter_sieve(Array, Current, Upper) ->
    filter_sieve(wipe_values(Array, Current, Current + Current, Upper),
                 Current + 1, Upper).

wipe_values(Array, _Base, Next, Upper) when Next > Upper ->
    Array;
wipe_values(Array, Base, Next, Upper) ->
    wipe_values(array:set(Next, 0, Array), Base, Next + Base, Upper).



strip(_N, [], NewList) ->
    lists:reverse(NewList);
strip(N, [H | T], NewList) when H rem N =:= 0 ->
    strip(N, T, NewList);
strip(N, [H | T], NewList) ->
    strip(N, T, [H | NewList]).




%% -spec find_primes(non_neg_integer(), list(non_neg_integer())) -> list(non_neg_integer()).
%% find_primes(0, Primes) ->
%%     lists:reverse(Primes);
%% find_primes(N, Primes) ->
%%     find_primes(N - 1, append_if_prime(N, Primes)).

%% -spec filter_sieve(list(non_neg_integer()), list(non_neg_integer())) -> list(non_neg_integer()).
%% filter_sieve(Primes, _Filtered) ->
%%     Primes.
