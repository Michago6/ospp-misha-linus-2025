%% @doc A server that keeps track of  <a target="_blank"
%% href="https://www.rd.com/culture/ablaut-reduplication/">ablaut
%% reduplication</a> pairs. You should implement two versions of the server. One
%% stateless server and one stateful server.
%%
%% <ul>
%% <li>
%% The stateless server keeps
%% track of a static number of ablaut reduplication pairs. Each pair is handled
%% by a separate message receive pattern.
%% </li>
%% <li>
%% The stateful server keeps
%% track of dynamic number of ablaut reduplication pairs using a <a
%% target="_blank" href="https://erlang.org/doc/man/maps.html">Map</a>.
%% </li>
%% </ul>
%% <p>
%% You should also implement process supervision of the server.
%% <ul>
%% <li>
%% The supervisor should <a target="_blank"
%% href="https://erlang.org/doc/reference_manual/processes.html#registered-processes">register</a>
%% the server process under the name `server'.
%% </li>
%% <li>
%% The name of a registered process can be used instead of the Pid when sending
%% messages to the process.
%% </li>
%% <li>
%% The supervisor should restart the server if the server terminates due to an
%% error.
%% </li>
%% </ul>
%% </p>

-module(server).
-export([start/2, update/0, update/1, stop/0, stop/1, loop/0, loop/1]).

%% @doc The initial state of the stateful server.

-spec pairs() -> map().

pairs() ->
    #{ping => pong,
      tick => tock,
      hipp => hopp,
      ding => dong,
      king => kong,
      bing => bong}.

%% @doc Starts the server.

-spec start(Stateful, Supervised) -> Server when
      Stateful :: boolean(),
      Supervised :: boolean(),
      Server :: pid().

start(false, false) ->
    spawn(fun() -> loop() end);
start(false, true) ->
    spawn(fun() -> supervisor(false) end);
start(true, false) ->
    spawn(fun() -> loop(pairs()) end);
start(true, true) ->
    spawn(fun() -> supervisor(true) end).

%% @doc The server supervisor. The supervisor must trap exit, spawn the server
%% process, link to the server process and wait the server to terminate. If the
%% server terminates due to an error, the supervisor should make a recursive
%% call to it self to restart the server.

-spec supervisor(Stateful) -> ok when
      Stateful :: boolean().

supervisor(Stateful) ->
    %% TODO: implement this 
    process_flag(trap_exit, true),
    case Stateful of 
        true ->
            spawn_link(fun() -> loop(pairs()) end);
        false ->
            spawn_link(fun() -> loop() end)
    end,
    receive 
    {'EXIT', PID, simulated_bug} ->
        io:format("simulated bug PID: ~p~n", [PID]),
        supervisor(Stateful)
    end.




%% @doc Terminates the supervised server.

-spec stop() -> ok | error.

stop() ->
    stop(server).

-spec stop(Server) -> ok | error when
      Server :: pid().

%% @doc Terminates the unsupervised server.

stop(Server) ->
    Server ! {stop, self()},
    receive
        {stop, ok} ->
            ok;
        Msg ->
            io:format("stop/1: Unknown message: ~p~n", [Msg]),
            error
    end.

%% @doc Makes the supervised server perform a hot code swap.

-spec update() -> ok | error.

update() ->
    update(server).

%% @doc Makes the unsupervised server perform a hot code swap.

-spec update(Server) -> ok | error when
      Server :: pid().

update(Server) ->
    Server ! {update, self()},
    receive
        {update, ok} ->
            ok;
        Msg ->
            io:format("update/1: Unknown message: ~p~n", [Msg]),
            error
    end.

%% @doc The process loop for the stateless server. The stateless server keeps
%% track of a static number of ablaut reduplication pairs. Each pair is handled
%% by a separate message receive pattern.

-spec loop() -> {stop, ok}.

loop() ->
    receive
        {ping, blipp, From} ->
            exit(simulated_bug),
            From ! {pong, blopp},
            loop();
        {ping, ding, From} ->
            From ! {pong, dong},
            loop();
        {ping, king, From} ->
            From ! {pong, kong},
            loop();
        {ping, ping, From} ->
            From ! {pong, pong},
            loop();
        {ping, bing, From} ->
            From ! {pong, bong},
            loop();
        {ping, tick, From} ->
            From ! {pong, tock},
            loop();
        {stop, From} ->
            From ! {stop, ok};
        {update, From}  ->
            %% TODO: Trigger a hot code swap.
            From ! {update, ok},
            ?MODULE:loop();
        Msg ->
            io:format("loop/0: Unknown message: ~p~n", [Msg]),
            loop()
    end.


%% @doc The process loop for the statefull server. The stateful server keeps
%% track of dynamic number of ablaut reduplication pairs using a <a
%% target="_blank" href="https://erlang.org/doc/man/Pairss.html">Map</a>.

-spec loop(Pairs) -> {stop, ok} when
      Pairs :: map().

loop(Pairs) ->
    receive
        {ping, flip, From} ->
            exit(simulated_bug);
        {ping, Ping, From} ->
            case maps:is_key(Ping, Pairs) of
                true ->
                    From ! {pong, maps:get(Ping, Pairs)};
                false ->
                    From ! unknown
            end,
            loop(Pairs);
        %% TODO: Handle the update, put and stop actions. 
        {stop, PID} ->
            update,
            PID ! {stop, ok};
            
        {update, From} ->
            From ! {update, ok},
            ?MODULE:loop(Pairs);
        {put, Ping, Pong, PID} ->
            PID ! {put, Ping, Pong, ok},
            Pair2 = maps:put(Ping, Pong, Pairs),
            loop(Pair2);
        Msg ->
            io:format("loop2/0: Unknown message: ~p~n", [Msg]),
            stop(),
            loop(Pairs)
    end.
