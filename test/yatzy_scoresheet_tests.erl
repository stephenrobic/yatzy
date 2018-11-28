-module(yatzy_scoresheet_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

setup()->
	S = yatzy_scoresheet:new(),
	{ok, S1} = yatzy_scoresheet:fill(ones, [1,1,2,2,3], S),
	{ok, S2} = yatzy_scoresheet:fill(twos, [1,2,2,2,3], S1),
	{ok, S3} = yatzy_scoresheet:fill(full_house, [3,2,2,2,3], S2),
	{ok, S4} = yatzy_scoresheet:fill(threes, [1,3,3,3,3], S3),
	{ok, S5} = yatzy_scoresheet:fill(fours, [1,4,4,4,4], S4),
	{ok, S6} = yatzy_scoresheet:fill(fives, [1,5,5,3,3], S5),
	{ok, S7} = yatzy_scoresheet:fill(sixes, [1,6,6,6,6], S6),
	{ok, S8} = yatzy_scoresheet:fill(one_pair, [2,2,1,3,4], S7),
	{ok, S9} = yatzy_scoresheet:fill(two_pairs, [2,2,4,4,1], S8),
	{ok, S10} = yatzy_scoresheet:fill(three_of_a_kind, [3,3,3,1,1], S9),
	{ok, S11} = yatzy_scoresheet:fill(four_of_a_kind, [5,5,5,5,1], S10),
	{ok, S12} = yatzy_scoresheet:fill(yatzy, [1,1,1,1,1], S11),
	{ok, S13} = yatzy_scoresheet:fill(small_straight, [1,2,3,4,5], S12),
	{ok, S14} = yatzy_scoresheet:fill(chance, [1,2,3,4,1], S13),
	{ok, S15} = yatzy_scoresheet:fill(large_straight, [2,3,4,5,6], S14),
	S15.

fill_test() ->
	Sheet = setup(),
	?assertEqual(already_filled, yatzy_scoresheet:fill(twos, [1,2,2,2,2], Sheet)),
	?assertEqual(already_filled, yatzy_scoresheet:fill(threes, [1,3,2,2,2], Sheet)),
	?assertEqual(already_filled, yatzy_scoresheet:fill(fours, [1,4,2,2,2], Sheet)),
	?assertEqual(already_filled, yatzy_scoresheet:fill(fives, [1,5,2,2,2], Sheet)),
	?assertEqual(already_filled, yatzy_scoresheet:fill(sixes, [1,6,2,2,2], Sheet)),
	?assertEqual(already_filled, yatzy_scoresheet:fill(twos, [1,2,2,2,2], Sheet)).


get_test() ->
	Sheet = setup(),
	?assertEqual({filled, 2}, yatzy_scoresheet:get(ones,Sheet)),
	?assertEqual({filled, 6}, yatzy_scoresheet:get(twos,Sheet)),
	?assertEqual({filled, 12}, yatzy_scoresheet:get(threes,Sheet)),
	?assertEqual({filled, 16}, yatzy_scoresheet:get(fours,Sheet)),
	?assertEqual({filled, 10}, yatzy_scoresheet:get(fives,Sheet)),
	?assertEqual({filled, 24}, yatzy_scoresheet:get(sixes,Sheet)),
	?assertEqual({filled, 4}, yatzy_scoresheet:get(one_pair,Sheet)),
	?assertEqual({filled, 12}, yatzy_scoresheet:get(two_pairs,Sheet)),
	?assertEqual({filled, 9}, yatzy_scoresheet:get(three_of_a_kind,Sheet)),
	?assertEqual({filled, 20}, yatzy_scoresheet:get(four_of_a_kind,Sheet)),
	?assertEqual({filled, 50}, yatzy_scoresheet:get(yatzy,Sheet)),
	?assertEqual({filled, 15}, yatzy_scoresheet:get(small_straight,Sheet)),
	?assertEqual({filled, 20}, yatzy_scoresheet:get(large_straight,Sheet)),
	?assertEqual({filled, 11}, yatzy_scoresheet:get(chance,Sheet)),
	?assertEqual({filled, 12}, yatzy_scoresheet:get(full_house,Sheet)).

upper_total_test()->
	Sheet = setup(),
	?assertEqual(70, yatzy_scoresheet:upper_total(Sheet)).

bonus_test()->
	Sheet = setup(),
	?assertEqual(50, yatzy_scoresheet:bonus(Sheet)).

lower_total_test()->
	Sheet = setup(),
	?assertEqual(153, yatzy_scoresheet:lower_total(Sheet)).

total_test()->
	Sheet = setup(),
	?assertEqual(273, yatzy_scoresheet:total(Sheet)).