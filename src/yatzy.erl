-module(yatzy).

-type slot() :: 'ones' | 'twos' | 'threes' | 'fours' | 'fives' | 'sixes'.

-type slot_type() :: 'upper' | 'lower'.

-type roll() :: [1..6]. 

 