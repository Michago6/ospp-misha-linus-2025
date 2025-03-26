%% @doc A functional FIFO queue.

%% @author Karl Marklund <karl.marklund@it.uu.se>

-module(fifo).
-export([new/0, size/1, push/2, pop/1, empty/1]).

%% To use EUnit we must include this:
-include_lib("eunit/include/eunit.hrl").

%% @doc Creates an empty FIFO buffer.
-opaque fifo()::{fifo, list(), list()}.
-export_type([fifo/0]).
-spec new() -> fifo().

%% Represent the FIFO using a 3-tuple {fifo, In, Out} where In and
%% Outs are lists.

new() -> {fifo, [], []}.

%% @doc Returns the size for a given FIFO queue.
-spec size(Fifo) -> integer() when
      Fifo::fifo().

size({fifo, In, Out}) ->
    length(In) + length(Out).

%% @doc Pushes one element to the FIFO In list.
-spec push({fifo, L1, L2}, H) -> {fifo, L1, L2}
    when H :: integer(),
         L1 :: [integer()],
         L2 :: [integer()].

%% To make it fast to push new values, add a new value to the head of
%% In.

push({fifo, In, Out}, X) ->
    {fifo, [X | In], Out}.

%% @doc Pops one element from the Out list if there is at least one element in it.
%%      If Out is empty, In is reversed and put into Out then Pops out. 
%% @throws 'empty fifo'
-spec pop({fifo, L1, L2}) -> {H, {fifo, L1, L2}}
    when H :: integer(),
         L1 :: [integer()],
         L2 :: [integer()].

%% pop should return {Value, NewFifo}

pop({fifo, [], []}) ->
    erlang:error('empty fifo');

%% To make pop fast we want to pop of the head of the Out list.

pop({fifo, In, [H|T]}) ->
    {H, {fifo, In, T}};

%% When Out is empty, we must take a performance penalty. Use the
%% reverse of In as the new Out and an empty lists as the new In, then
%% pop as usual.

pop({fifo, In, []}) ->
    L = lists:reverse(In),
    pop({fifo, [], L}).


%% @doc Checks if the given FIFO is empty. Returns true or false depending on
%% whether the FIFO is empty of not.
-spec empty(Fifo) -> boolean() when Fifo::fifo().

empty({fifo, [], []}) ->
    true;
empty({fifo, _, _}) ->
    false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% Eunit test cases  %%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% EUnit adds the fifo:test() function to this module.

%% All functions with names ending wiht _test() or _test_() will be
%% called automatically by fifo:test()


new_test_() ->
    [?_assertEqual({fifo, [], []}, new()),
     ?_assertMatch(0, fifo:size(new())),
     ?_assertException(error, 'empty fifo', pop(new()))].

push_test() ->
    push(new(), a).

push_pop_test() ->
    ?assertMatch({a,_}, pop(push(new(), a))).


f1() ->
    push(push(push(new(), foo), bar), "Ahloa!").

size_test_() ->
    F1 = f1(),
    F2 = push(F1, atom),
    {_, F3} = fifo:pop(F2),

    [?_assertMatch(3, fifo:size(F1)),
     ?_assertMatch(4, fifo:size(F2)),
     ?_assertMatch(3, fifo:size(F3))].


push_test_() ->
    F1 = f1(),
    F2 = push(f1(), last),

    [ ?_assertMatch(1, fifo:size(fifo:push(fifo:new(), a))),
      ?_assertEqual(fifo:size(F1) + 1, fifo:size(F2))].

empty_test_() ->
    F = f1(),
    {_, F2} = pop(F),
    {_, F3} = pop(F2),
    {_, F4} = pop(F3),

    [?_assertMatch(true, empty(new())),
     ?_assertMatch(false, empty(F)),
     ?_assertMatch(false, empty(F2)),
     ?_assertMatch(false, empty(F3)),
     ?_assertMatch(true, empty(F4))].
