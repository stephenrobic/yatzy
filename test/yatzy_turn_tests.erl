-module(yatzy_turn_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

roll_valid_test() ->
	{_, A} = yatzy_turn:start(),
	R = yatzy_turn:dice(A),
	K = lists:sublist(R, 1, 4),
	?assertEqual({ok, R}, yatzy_turn:roll(A, K)),
	?assertEqual(invalid_keepers, yatzy_turn:roll(A, [6,6,6,6,6])),
	?assertEqual(invalid_keepers, yatzy_turn:roll(A, [6,6,5,6,6])),
	?assertEqual(invalid_keepers, yatzy_turn:roll(A, [1,1,1,1,1])),
	R1 = yatzy_turn:dice(A),
	K1 = lists:sublist(R1, 1, 5),
	?assertEqual({ok, R1}, yatzy_turn:roll(A, K1)),
	?assertEqual(finished, yatzy_turn:roll(A, [])),
	?assertEqual(finished, yatzy_turn:roll(A, [])),

	{_, B} = yatzy_turn:start(),
	R2 = yatzy_turn:dice(B),
	K2 = lists:sublist(R2, 1, 5),

	
	?assertEqual({ok, R2}, yatzy_turn:roll(B, K2)),
	?assertEqual(invalid_keepers, yatzy_turn:roll(B, [1,2,3,4,5])),
	?assertEqual(invalid_keepers, yatzy_turn:roll(B, [2,3,4,5,6])),
	R3 = yatzy_turn:dice(B),
	K3 = lists:sublist(R3, 1, 5),

	?assertEqual({ok, R3}, yatzy_turn:roll(B, K3)),
	?assertEqual(finished, yatzy_turn:roll(B, [])),
	?assertEqual(finished, yatzy_turn:roll(B, [])),

	yatzy_turn:stop(A),
	yatzy_turn:stop(B).

evil_roll_test() ->
	[evil_test_gen() || _ <- lists:seq(1, 100)].

evil_test_gen() ->
	{_, A} = yatzy_turn:start(),
	R = yatzy_turn:dice(A),
	K = lists:sublist(R, 1, 4),
	yatzy_turn:roll(A, K),
	R1 = yatzy_turn:dice(A),
	yatzy_turn:stop(A),
	?assertEqual(K--R1, []).

% dice_test() ->
% 	A = yatzy_turn:start(),
% 	yatzy_turn:stop(A).

% stop_test() ->
% 	A = yatzy_turn:start(),
% 	yatzy_turn:stop(A).