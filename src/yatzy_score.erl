-module(yatzy_score).

-export([calc/2]).

-spec calc(yatzy:slot(), yatzy:roll()) -> non_neg_integer(). 

calc(chance, Roll) ->
    lists:sum(Roll);
calc(ones, Roll) ->
    helper(Roll, 1);
calc(twos, Roll) ->
    helper(Roll, 2);
calc(threes, Roll) ->
    helper(Roll, 3);
calc(fours, Roll) ->
    helper(Roll, 4);
calc(fives, Roll) ->
    helper(Roll, 5);
calc(sixes, Roll) ->
    helper(Roll, 6);
calc(three_of_a_kind, Roll) ->
    three_of_a_kind(lists:sort(Roll));

calc(four_of_a_kind, Roll) ->
    four_of_a_kind(lists:sort(Roll));

calc(yatzy, Roll) ->
    yatzy(lists:sort(Roll));

calc(full_house, Roll) ->
    full_house(lists:sort(Roll));

calc(small_straight, Roll) ->
    small_straight(lists:sort(Roll));

calc(large_straight, Roll) ->
    large_straight(lists:sort(Roll));

calc(one_pair, Roll) ->
    one_pair(lists:sort(Roll));

calc(two_pairs, Roll) ->
    two_pairs(lists:sort(Roll)).

helper(Roll, Number) -> 
    lists:sum(lists:filter(fun(X) -> X =:= Number end, Roll)).

three_of_a_kind([A,A,A,_,_]) ->
    3 * A;
three_of_a_kind([_,A,A,A,_]) ->
    3 * A;
three_of_a_kind([_,_,A,A,A]) ->
    3 * A;
three_of_a_kind(_) ->
    0.

four_of_a_kind([A,A,A,A,_]) ->
    4 * A;
four_of_a_kind([_,A,A,A,A]) ->
    4 * A;
four_of_a_kind(_) ->
    0.

full_house([A,A,B,B,B]) when A /= B -> 
    (A * 2) + (B * 3);
full_house([A,A,A,B,B]) when A /= B-> 
    (A * 3) + (B * 2);
full_house(_) ->
    0.

yatzy([A,A,A,A,A]) ->
    50;
yatzy(_) ->
    0.

small_straight([1, 2, 3, 4, 5]) ->
    15;
small_straight(_) ->
    0.

large_straight([2, 3, 4, 5, 6]) ->
    20;
large_straight(_) ->
    0.

two_pairs([A, A, B, B, _]) when A /= B->
    2 * A + 2 * B;
two_pairs([A, A, _, B, B]) when A /= B->
    2 * A + 2 * B;
two_pairs([_, A, A, B, B]) when A /= B->
    2 * A + 2 * B;
two_pairs(_) ->
    0.

one_pair([_, _, A, A, _]) ->
    2 * A;
one_pair([_, _, _, A, A]) ->
    2 * A;
one_pair([A, A, _, _, _]) ->
    2 * A;
one_pair([_, A, A, _, _]) ->
    2 * A;
one_pair(_) ->
    0.

