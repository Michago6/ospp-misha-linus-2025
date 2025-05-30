-module(worker).

-export([start/4]).

%% @doc Starts a worker process. The worker will make random guesses between
%% `Min' and `Max'.

-spec start(Server, Master, Min, Max) -> Worker when
      Server :: pid(), 
      Master :: pid(),
      Min :: number(), 
      Max :: number(),
      Worker :: pid().

start(Server, Master, Min, Max) ->
   spawn(fun() -> loop(Server, Master, Min, Max, 0) end).


loop(Server, Master, Min, Max, Guesses) ->
    process_flag(trap_exit, true),
    Guess = utils:random(Min, Max),
    Server ! {guess, Guess, self()},

    % Log Guess
    Master ! {receive_worker_data, Guess, Guesses, self()},

    receive
        {right, Guess} ->
            io:format("~p ~*.. B~n", [self(), utils:width(Max), Guess]),
            % io:format("~p I win :)~n", [self()]),
            % Master ! print,
            Master ! {winner, Guess, Guesses, self()};
            % todo: send message to master???
            % loop(Server, Master, Min, Max, Guesses);
        {wrong, Guess} ->
            io:format("~p ~*.. B~n", [self(), utils:width(Max), Guess]),
            loop(Server, Master, Min, Max, Guesses + 1);
        {'EXIT', _From, loser} ->
            timer:sleep(500),
            io:format("~p I lose :(~n", [self()]),
            exit(finished)
    end.
