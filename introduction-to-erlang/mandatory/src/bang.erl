%% @doc Process supervision. To study process supervision you will construct an
%% Erlang system with one supervisor process and one worker process called bang.

%% @author Karl Marklund <karl.marklund@it.uu.se>

-module(bang).
-export([start/0]).


%% @doc Start the system. 
-spec start() -> ok. 
start() ->
    io:format("~nSupervisor with PID ~p started~n", [self()]),

    %% TODO: trap the exit signal.

    process_flag(trap_exit, true),
    Counter = 5,
    start_bang(Counter),
    supervisor_loop(Counter).

start_bang(Counter) ->
    Supervisor = self(),
    PID = spawn_link(fun() -> bang(Supervisor, Counter) end),
    io:format("bang(~w) with PID ~p started~n", [Counter, PID]).

counter_msg(Counter) when Counter rem 2 == 1 -> tick;
counter_msg(_Counter) -> tock.

supervisor_loop(Counter) ->
    receive
        {countdown, N} ->
            io:format("~w ~s~n", [N, counter_msg(N)]),
            supervisor_loop(N - 1);
        {'EXIT', PID, Reason} ->
            % io:format("Process ~w terminated with reason ~w!~n", [PID, Reason]),
            case Counter > 0 of 
                true ->
                    io:format("bang(~w) with PID ~w died~n", [Counter, PID]),
                    timer:sleep(1000),
                    N = Counter,
                    start_bang(Counter),
                    timer:sleep(1000),
                    supervisor_loop(N);
                false ->
                    io:format(">>BANG<<~n")
            end
    end.

bang(Supervisor, 0) ->
    Supervisor ! {countdown, 0},
    timer:sleep(1000),
    exit(bang);
bang(Supervisor, Counter) ->
    timer:sleep(1000),
    death:gamble(0.3),
    Supervisor ! {countdown, Counter},
    bang(Supervisor, Counter - 1).
