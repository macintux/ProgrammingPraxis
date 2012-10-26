%%% @author John Daily <jd@epep.us>
%%% @copyright (C) 2012, John Daily
%%% @doc
%%%   Assignment from programmingpraxis.com: Bingo
%%%
%%% @reference See <a href="http://programmingpraxis.com/2009/02/19/bingo/">Bingo</a>.
%%% @end
%%% Created : 25 Oct 2012 by John Daily <jd@epep.us>

-module(bingo).
-compile(export_all).

%% Function type to generate an integer between the lower and upper bounds
-type genfun() :: fun((non_neg_integer(), non_neg_integer(), list(non_neg_integer())) ->
                              { non_neg_integer(), list(non_neg_integer()) }).
-type card() :: list(column()).
-type column() :: list(non_neg_integer()).

-spec build_card(genfun()) -> card().
build_card(Generator) ->
    build_card_aux(Generator, 1, 15, 15, 5, []).

-spec build_card_aux(genfun(), non_neg_integer(), non_neg_integer(),
                     non_neg_integer(), non_neg_integer(), card()) -> card().
build_card_aux(_Generator, _Min, _Max, _Incr, 0, Columns) ->
    lists:reverse(Columns);
build_card_aux(Generator, Min, Max, Incr, ColNo, Columns) ->
    build_card_aux(Generator, Min + Incr, Max + Incr, Incr, ColNo - 1,
                   [ build_column(Generator, [], Min, Max, ColNo =:= 3, []) | Columns ]).

%% Regrettably, we have to use very awkward special case handling to
%% make sure we introduce a 0 as a wildcard value in the center of the
%% board.  The Wildcard value is true when this is the 3rd column.
-spec build_column(genfun(), list(non_neg_integer()), non_neg_integer(), non_neg_integer(), boolean(), column()) -> column().
build_column(_Generator, _Vals, _Min, _Max, _Wildcard, Rows) when length(Rows) >= 5 ->
    Rows; %% Could reverse, but why bother?
build_column(Generator, Vals, Min, Max, true, Rows) when length(Rows) =:= 2 ->
    build_column(Generator, Vals, Min, Max, true, [ 0 | Rows ]); %% Wildcard (0) value in center
build_column(Generator, Vals, Min, Max, Wildcard, Rows) ->
    { NewVal, RemainingVals } = Generator(Min, Max, Vals),
    build_column(Generator, RemainingVals, Min, Max, Wildcard, [ NewVal | Rows ]).

%%
%% Function is a bit more complex than it would need to be if we
%% didn't have to prevent repeating values.
-spec random_generator() -> genfun().
random_generator() ->
    { A1, A2, A3 } = now(),
    random:seed(A1, A2, A3),
    fun(X, Y, []) ->
            Vals = lists:seq(X, Y),
            { List1, [H | T] } = lists:split(random:uniform(length(Vals)) - 1, Vals),
            { H, List1 ++ T };
       (_X, _Y, Vals) ->
            { List1, [H | T] } = lists:split(random:uniform(length(Vals)) - 1, Vals),
            { H, List1 ++ T }
    end.
