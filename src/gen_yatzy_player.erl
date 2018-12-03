-module(gen_yatzy_player).
-behavior(gen_server).

-export([start_link/1, stop/1]). %start_link/0,
-export([fill/3, sheet/1]).
-export([init/1, handle_call/3, handle_cast/2]).

-spec start_link(Name::atom()) -> {ok, pid()}.
-spec fill(Name::atom(), yatzy:slot(), yatzy:roll()) -> {ok, Score::integer()} | {error, Reason::any()}.
-spec sheet(Name::atom()) -> yatzy_scoresheet:t().

%%Operation and Maintenance API

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, na, []).

stop(Name) ->
    gen_server:cast(Name, stop).



%Yatzy_player API
fill(Name, Slot, Roll) ->
    gen_server:call(Name, {fill, Slot, Roll}).

sheet(Name) ->
    gen_server:call(Name, {sheet, Name}).



%Callback Functions
init(na) ->
    Sheet = yatzy_scoresheet:new(),
    {ok, Sheet}.

handle_cast(stop, LoopData) ->
    {stop, normal, LoopData}.
     
handle_call({fill, Slot, Roll}, _From, Sheet) ->
    Reply = yatzy_scoresheet:fill(Slot, Roll, Sheet),
    case Reply of
        {ok, NewSheet} -> 
            {filled, Score} = yatzy_scoresheet:get(Slot, NewSheet),
            {reply, {ok,Score}, NewSheet};
        Reason ->
            {reply, {error, Reason}, Sheet}
    end;

handle_call({sheet, Name}, _From, Sheet) ->
    {reply, Sheet, Sheet}.



    
    %% API
   % -export([start/1, stop/1, start_link/1]).
    %-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
    %-record(state, {dummy}).
    %start(Name) ->
    %   _sup:start_child(Name).
    
    %stop(Name) ->
    %   gen_server:call(Name, stop).
    
    %start_link(Name) ->
    %   gen_server:start_link({local, Name}, ?MODULE, [], []).
    
    %init(_Args) ->
    %   {ok, #state{dummy=1}}.
    
    %handle_call(stop, _From, State) ->
    %   {stop, normal, stopped, State};
    
    %handle_call(_Request, _From, State) ->
    %    {reply, ok, State}.
    
    %handle_cast(_Msg, State) ->
    %   {noreply, State}.
    
    %handle_info(_Info, State) ->
    %   {noreply, State}.
    
    %terminate(_Reason, _State) ->
    %   ok.
    
    %code_change(_OldVsn, State, _Extra) ->
    %   {ok, State}.
    
    
    
    
    
    