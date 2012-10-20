%%% @author John Daily <jd@epep.us>
%%% @copyright (C) 2012, John Daily
%%% @doc
%%%   Assignment from programmingpraxis.com
%%%
%%% @reference See <a href="http://programmingpraxis.com/2009/02/19/rpn-calculator/">RPN Calculator</a>.
%%% @end

-module(rpncalc).
-export([rpn/0]).

-type io_return() :: { atom(), string() } | 'eof'.
-type operator() :: { 'operator', atom() }.
-type token() :: operator() | integer() | float() | 'error'.

-type operator_match() :: fun((string()) -> operator()).
-type matcher() :: { tuple(), operator_match() }.

-record(regexes,
        { operator = re_return("^\\s*(\\*|\\+|-|/)\\s*$") :: tuple(),
          integer  = re_return("^\\s*(\\d+)\\s*$") :: tuple(),
          float    = re_return("^\\s*(\\d+\\.\\d+)\\s*$") :: tuple()
        }).
-type regexes() :: #regexes{}.

-spec rpn() -> 'done'.
rpn() ->
    process_line(file:read_line(standard_io), #regexes{}).

-spec process_line(io_return(), regexes()) -> 'done'.
process_line(eof, _R) ->
    done;
process_line({error, Why}, _R) ->
    io:format("Exiting: ~p~n", [ Why ]),
    done;
process_line({ok, Line}, R) ->
    Tokens = parse_tokens(string:tokens(chomp(Line), " \t"), [], R),
    io:format("~p~n", [ calculate(Tokens, []) ]),
    process_line(file:read_line(standard_io), R).

-spec calculate(list(token()), list(number())) -> number().
calculate([], []) ->
    0;
calculate([], [ Tally | [] ]) ->
    Tally;
calculate([ V | T ], Stack) when is_number(V) ->
    calculate(T, [ V | Stack ]);
calculate([ { operator, add } | T], [ V2, V1 | StackTail ]) ->
    calculate(T, [ V1 + V2 | StackTail]);
calculate([ { operator, subtract } | T], [ V2, V1 | StackTail ]) ->
    calculate(T, [ V1 - V2 | StackTail]);
calculate([ { operator, multiply } | T], [ V2, V1 | StackTail ]) ->
    calculate(T, [ V1 * V2 | StackTail]);
calculate([ { operator, divide } | T], [ V2, V1 | StackTail ]) ->
    calculate(T, [ V1 / V2 | StackTail]);
calculate([ { operator, _ } | _T], _) ->
    error.




-spec parse_tokens(list(string()), list(token()), regexes()) -> list(token()).
%% Turn a list of string tokens into a list of token().
%% Should consider making this a stream operation.
parse_tokens([], Stack, _Regexes) ->
    lists:reverse(Stack);
parse_tokens([H | T], Stack, Regexes) ->
    parse_tokens(T, [ parse_one_token(H, Regexes) | Stack ], Regexes).

-spec parse_one_token(string(), regexes()) -> token().
parse_one_token(Token, #regexes{operator = Operator, integer = Integer, float = Float}) ->
    try_regex(Token, [ { Operator, fun match_operator/1 },
                       { Integer, { erlang, list_to_integer } },
                       { Float, { erlang, list_to_float } } ]).

-spec try_regex(string(), [ matcher() ]) -> token().
try_regex(_, []) ->
    error;
try_regex(Token, [{ Regex, Fun } | T]) ->
    case re:run(Token, Regex, [ { capture, first, list } ]) of
        { match, [ Captured ]} ->
            Fun(Captured);
        _ ->
            try_regex(Token, T)
    end.

-spec match_operator(string()) -> operator().
match_operator([$*]) ->
    { operator, multiply };
match_operator([$-]) ->
    { operator, subtract };
match_operator([$+]) ->
    { operator, add };
match_operator([$/]) ->
    { operator, divide };
match_operator(_) ->
    error.




%%
%% re:compile returns a tuple, which I don't think can be dropped into
%% a record easily, so we use this auxiliary function
-spec re_return(string()) -> tuple().
re_return(Pattern) ->
    { ok, Re } = re:compile(Pattern),
    Re.

%%
%% Drop any end of line marker by splitting on them and returning the
%% first component
-spec chomp(string()) -> string().
chomp(String) ->
    chomp_match(string:tokens(String, "\r\n")).

-spec chomp_match(list()) -> string().
chomp_match([]) ->
    "";
chomp_match([Line | _Leftover]) ->
    Line.
