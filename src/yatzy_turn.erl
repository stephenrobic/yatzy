-module(yatzy_turn).

-export([start/0, roll/2, rolls_left/1, dice/1, stop/1]).

-spec start() -> {'ok', TurnPid::pid()}.
-spec roll(TurnPid::pid(), Keep::[1..6]) -> {'ok', yatzy:roll()} | 'invalid_keepers' | 'finished'.
%% Once the player has selected which dice to keep, roll the remaining dice unless they
%%have already been rolled twice.
-spec dice(TurnPid::pid()) -> yatzy:roll().
%% Just the rolled dice as it stands at this point.
-spec rolls_left(TurnPid::pid()) -> 0..2.
-spec stop(TurnPid::pid()) -> yatzy:roll().
%% Just stop the procees and get out what was rolled.

start() ->
    Roll = dice_roll(5),
    Pid = spawn(fun() ->
        first_roll(Roll)
        end),
    {ok, Pid}.

roll(TurnPid, Keep) ->
    TurnPid ! {self(), {roll, Keep}},
    receive
        Msg ->
            Msg
    end.

dice(TurnPid) ->
    TurnPid ! {self(), dice},
    receive
        Dice ->
            Dice
    end.

rolls_left(TurnPid) -> 
    TurnPid ! {self(), rolls_left},
    receive
        Rolls ->
            Rolls
    end.

stop(TurnPid) ->
    TurnPid ! {self(), stop},
    receive
        Stop ->
            Stop
    end.

first_roll(Roll) ->
    receive
        {From, {roll, Keep}} ->
            case valid_keepers(Keep, Roll) of 
                true ->
                    NewRoll = Keep ++ dice_roll(5 - length(Keep)),
                    From ! {ok, NewRoll},
                    second_roll(NewRoll);
                false ->
                    From ! invalid_keepers,
                    first_roll(Roll)
            end;
        {From, dice} ->
            From ! Roll,
            first_roll(Roll);
        {From, stop} ->
            From ! Roll,
            finished(Roll);
        {From, rolls_left} ->
            From ! 2,
            first_roll(Roll)
    end.

second_roll(Roll) ->
    receive
        {From, {roll, Keep}} ->
            case valid_keepers(Keep, Roll) of
                true ->
                    NewRoll = Keep ++ dice_roll(5 - length(Keep)),
                    From ! {ok, NewRoll},
                    finished(NewRoll);
                false ->
                    From ! invalid_keepers,
                    second_roll(Roll)
            end;
        {From, dice} ->
            From ! Roll,
            second_roll(Roll);
        {From, stop} ->
            From ! Roll,
            finished(Roll);
        {From, rolls_left} ->
            From ! 1,
            second_roll(Roll)
    end.

finished(Roll) ->
    receive
        {From, roll} ->
            From ! finished,
            finished(Roll);
        {From, dice} ->
            From ! Roll,
            finished(Roll);
        {From, stop} ->
            From ! Roll,
            finished(Roll);
        {From, rolls_left} ->
            From ! 0,
            finished(Roll)
    end.

dice_roll(HowMany) ->
    [rand:uniform(6) || _ <- lists:seq(1, HowMany)].

valid_keepers(Keep, Roll) ->
    case Keep -- Roll of
        [] ->
            true;
        _ ->
            false
    end.
