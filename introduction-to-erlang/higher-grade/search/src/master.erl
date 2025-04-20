-module(master).

-export([start/3, stop/1, log_guess/2]).

init() ->
    maps:new().

%% @doc Starts the server and `NumWorkers' workers. The server is started with a
%% random secret number between `Min' and `Max'.

-spec start(NumWorkers, Min, Max) -> Master when
      NumWorkers :: integer(),
      Min :: integer(),
      Max :: integer(),
      Master :: pid().

start(NumWorkers, Min, Max) ->
    Secret = utils:random(Min, Max),
    io:format("Secret: ~p~n", [Secret]),
    Server = server:start(Secret),
    Master = spawn(fun() -> loop(NumWorkers, init()) end),
    io:format("Master: ~p~n", [Master]),

    [worker:start(Server, Master, Min, Max) || _ <- lists:seq(1, NumWorkers)],

    % Master ! foo,
    % Master ! bar,

    Master.

%% @doc Stops the `Master'.

-spec stop(Master) -> stop when 
      Master :: pid().

stop(Master) ->
    Master ! stop.

loop(0, Map) ->
    io:format("DONE ~p~n", [Map]);

loop(CountDown, Map) ->
    % io:format("Master Tick~n"),
    receive 
        {guess, _Master} ->
            loop(CountDown, Map);
        {receive_worker_data, Guess, Guesses, Self} ->
            % io:format("Message received~n"),
            NewMap = maps:put(Self, {Guesses, Guess, searching}, Map),
            loop(CountDown, NewMap);
        {winner, Guess, Guesses, Self} ->
            NewMap = maps:put(Self, {Guesses, Guess, winner}, Map),
            terminate(NewMap);
        print ->
            io:format("Map:~n~p~n", [Map]),
            loop(CountDown, Map);
        stop ->
            io:format("Stop!~n");
        Msg ->
            io:format("master:loop/2 Unknown message ~p~n", [Msg]),
            loop(CountDown, Map);
        _ ->
            io:format("master:loop/2 Unknown message (nestled receive)~n"),
            loop(CountDown, Map)
    end.
-spec log_guess(Master, Self) -> ok when
      Master :: pid(),
      Self :: pid().

log_guess(Master, Self) ->
    Self ! {request_worker_data, Master},
    % io:format("Sent message~n"),
    ok.

% go through the map and terminate all workers that are still searching
terminate(Map) ->
    Fun = fun(Key, Value) ->
        % Key * 2,
        {X, Y, State} = Value,
        case State of
            winner ->
                {X, Y, winner};
            searching ->
                {X, Y, loser}
        end
    end,
         
    FinalMap = maps:map(Fun, Map),
    stop(self()),
    lists:foreach(fun(PID) -> PID ! {'EXIT', self(), loser} end, maps:keys(FinalMap)),
    timer:sleep(1000),
    io:format("Final statistics from the master:~n~p~n", [FinalMap]).