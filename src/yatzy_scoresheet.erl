-module(yatzy_scoresheet).

-type t() :: map().

-export([new/0,fill/3,get/2,upper_total/1,bonus/1,lower_total/1,total/1]).

-spec new() -> t().
-spec fill(yatzy:slot(), yatzy:roll(), t()) -> {'ok', t()} | 'already_filled' | 'invalid_slot'.
-spec get(yatzy:slot(), t()) -> {'filled', non_neg_integer()} | 'invalid_slot' | 'empty'.
-spec upper_total(t()) -> non_neg_integer().
-spec bonus(t()) -> 0 | 50.
-spec lower_total(t()) -> non_neg_integer().
-spec total(t()) -> non_neg_integer().

new() -> 
    maps:new().

fill(Slot, Roll, Sheet) ->
    case is_valid_slot(Slot) of
        false -> 
            'invalid_slot';
        true -> 
            case maps:get(Slot, Sheet, 'empty') of 
                'empty' ->
                    Score = yatzy_score:calc(Slot, Roll),
                    NewSheet = maps:put(Slot, Score, Sheet),
                    {'ok', NewSheet};                    
                _ ->
                    'already_filled'
            end
    end.

get(Slot, Sheet) ->
    case is_valid_slot(Slot) of
        false ->
            'invalid_slot';
        true ->
            case maps:get(Slot, Sheet, 'empty') of 
                'empty' ->
                    'empty';                   
                _ ->
                    {filled, maps:get(Slot, Sheet)}
            end
    end.

upper_total(Sheet) ->
    maps:get(ones, Sheet, 0) + maps:get(twos, Sheet, 0) + maps:get(threes, Sheet, 0) + maps:get(fours, Sheet, 0) + maps:get(fives, Sheet, 0) + maps:get(sixes, Sheet, 0). 

lower_total(Sheet) ->
    maps:get(three_of_a_kind, Sheet, 0) + maps:get(four_of_a_kind, Sheet, 0) + maps:get(full_house, Sheet, 0) + maps:get(small_straight, Sheet, 0) + maps:get(large_straight, Sheet, 0) + maps:get(yatzy, Sheet, 0) + maps:get(chance, Sheet, 0) + maps:get(one_pair, Sheet, 0) + + maps:get(two_pairs, Sheet, 0).

bonus(Sheet) ->
    case upper_total(Sheet) > 62 of
        true -> 50;
        false -> 0
    end.

total(Sheet) ->
    lower_total(Sheet) + upper_total(Sheet) + bonus(Sheet).


is_valid_slot(X) ->
    lists:member(X,[chance, ones, twos, threes, fours, fives, sixes, three_of_a_kind, four_of_a_kind, yatzy, full_house, small_straight, large_straight, one_pair, two_pairs]).




