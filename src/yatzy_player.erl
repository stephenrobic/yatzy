-module(yatzy_player).

-export([new/1, fill/3, sheet/1]).

-spec new(Name::atom()) -> {ok, pid()}.
-spec fill(Name::atom(), yatzy:slot(), yatzy:roll()) -> {ok, Score::integer()} | {error, Reason::any()}.
-spec sheet(Name::atom()) -> yatzy_scoresheet:t().
%add a stop function

new(Name) ->
    Sheet = yatzy_scoresheet:new(),
    Pid = spawn(fun() ->
        loop(Sheet) 
        end),
    register(Name, Pid),
    {ok, Pid}.
    
fill(Name, Slot, Roll) ->
    Name ! {self(), {fill, Slot, Roll}},
    receive
        {filled, Score} ->
            {ok, Score};
        {error, Reason} ->
            {error, Reason}
    end.

sheet(Name) ->
    Name ! {self(), sheet},
    receive
        Sheet ->
            Sheet
    end.
    %request/receives player sheet from loop
    %receives first, then manipulates and returns afterwards

loop(Sheet) ->
    receive
       {From, {fill, Slot, Roll}} ->
            case yatzy_scoresheet:fill(Slot, Roll, Sheet) of
                {'ok', NewSheet} ->
                    {filled, Score} = yatzy_scoresheet:get(Slot, NewSheet),
                    From ! {filled, Score},
                    loop(NewSheet);
                Reason ->
                    From ! {error, Reason},
                    loop(Sheet)
            end;
        {From, sheet} ->
            From ! Sheet,
            loop(Sheet)
    end.

