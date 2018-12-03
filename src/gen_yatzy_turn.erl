-module(gen_yatzy_turn).
-behavior(gen_statem).

-export([start_link/0, stop/1]). %start_link/1,
-export([dice/1, roll/2, rolls_left/1]). %no start/0,
-export([init/1, callback_mode/0, terminate/3]).
-export([first_roll/3, second_roll/3, finished/3]).

%%Operation and Maintenance API
start_link()->
    gen_statem:start_link(?MODULE, na,[]).

%start_link(TurnPid) ->
%    gen_statem:start_link({local, TurnPid}, ?MODULE, na, []).

%%Yatzy_turn API
dice(TurnPid) ->
    gen_statem:call(TurnPid, dice).

roll(TurnPid, Keep) ->
    gen_statem:call(TurnPid, {roll, Keep}).

rolls_left(TurnPid) ->
    gen_statem:call(TurnPid, rolls_left).

stop(TurnPid) ->
    gen_statem:stop(TurnPid). % gen_statem:stop(TurnPid, stop).


%%Callback functions
init(na) ->
    Roll = dice_roll(5),
    {ok, first_roll, Roll}.

callback_mode() ->
    state_functions.

first_roll({call, From}, {roll, Keep}, Roll) ->
    case valid_keepers(Keep, Roll) of 
        true ->
            NewRoll = Keep ++ dice_roll(5 - length(Keep)),
            {next_state, second_roll, NewRoll, {reply, From, {ok, NewRoll}}};
        false ->
            {keep_state, first_roll, Roll, {reply, From, invalid_keepers}}
    end;
first_roll({call, From}, dice, Roll) -> 
    {keep_state_and_data, {reply, From, Roll}};
first_roll({call, From}, stop, Roll) ->
    {next_state, finished, Roll, {reply, From, Roll}};
first_roll({call, From}, rolls_left, Roll) ->
    {keep_state_and_data, {reply, From, 2}}.


second_roll({call, From}, {roll, Keep}, Roll) ->
    case valid_keepers(Keep, Roll) of 
        true ->
            NewRoll = Keep ++ dice_roll(5 - length(Keep)),
                {next_state, finished, NewRoll, {reply, From, {ok, NewRoll}}};
        false ->
            {keep_state, second_roll, Roll, {reply, From, invalid_keepers}}
    end;
second_roll({call, From}, dice, Roll) ->
    {keep_state_and_data, {reply, From, Roll}};
second_roll({call, From}, stop, Roll) ->
    {next_state, finished, Roll, {reply, From, Roll}};
second_roll({call, From}, rolls_left, Roll) ->
    {keep_state_and_data, {reply, From, 1}}.


finished({call, From}, {roll, Keep}, Roll) -> 
    {keep_state, finished, Roll, {reply, From, finished}};
finished({call, From}, dice, Roll) ->
    {keep_state_and_data, {reply, From, Roll}};
finished({call, From}, stop, Roll) ->
    {keep_state, finished, Roll, {reply, From, Roll}};
finished({call, From}, rolls_left, Roll) ->
    {keep_state_and_data, {reply, From, 0}}. %{next state, 

terminate(_Reason, State, Roll) -> %(_Reason, State, Data)
    State =/= locked, %exactly not equal to
    ok.


%%Helper functions
dice_roll(HowMany) ->
    [rand:uniform(6) || _ <- lists:seq(1, HowMany)].

valid_keepers(Keep, Roll) ->
    case Keep -- Roll of
        [] ->
            true;
        _ ->
            false
    end.