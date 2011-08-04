:- use_module(library(aspects), [aspect/3, aspect_declaration/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The following aspects read out the program state on close

:- aspect(before, command(Cmd, P, _),
	  (Cmd = exit -> format('%% Program state was ~p~n', [P]); true)).

:- aspect(around, instruction(_, P, _),
	  (prove_goal -> true; format('%% Program state was ~p~n', [P]))).

:- aspect(around, initialize_memory(Mem),
          Mem = memory(0, avl(cell(0, value(0)), empty, empty))).

:- use_module(library(avl_tree), [find_node/2, replace_node/4, add_node/3]).
:- aspect_declaration(use_module(library(avl_tree), [find_node/2, add_node/3,
						     replace_node/4])).

:- ensure_loaded(library(math)).
:- aspect_declaration(ensure_loaded(library(math))).

:- aspect(around, selected_memory_cell(Datum, P0, P1),
          (P0 = program(_, _, _, memory(Index, AVL), _),
           find_node(cell(Index, value(Datum)), AVL),
           P1 = P0)).

:- aspect(around, command(Cmd, P0, P1),
          (Cmd = store,
	   !,
	   P0 = program(A, H, R, memory(Index, AVL0), P),
	   get_active_ring(A, R, ring(_, _, _, Value)),
	   replace_node(cell(Index, _), cell(Index, Value), AVL0, AVL1),
	   P1 = program(A, H, R, memory(Index, AVL1), P))).

:- aspect(around, command(Cmd, P0, P1),
          (Cmd = dadd,
           P0 = program(ops, H, Rings, memory(Index, AVL0), P),
	   !,
	   get_active_ring(ops, Rings, ring(ops, _, _, value(X))),
	   floor(X, Y),
	   NewIndex is Index + Y,
	   (find_node(cell(NewIndex, _), AVL0) -> AVL1 = AVL0;
	    add_node(cell(NewIndex, value(0)), AVL0, AVL1)),
	   P1 = program(ops, H, Rings, memory(NewIndex, AVL1), P))).

:- aspect(around, command(Cmd, P0, P1),
	  (Cmd = intIO,
           P0 = program(ops, H, Rings, memory(Index, AVL0), P),
	   get_active_ring(ops, Rings, ring(ops, _, _, value(0))),
	   !,
	   read_term(N, []),
	   (integer(N) -> true; raise_exception(integral_expected(received(N)))),
	   replace_node(cell(Index, _), cell(Index, value(N)), AVL0, AVL1),
	   P1 = program(ops, H, Rings, memory(Index, AVL1), P))).

:- aspect(around, command(Cmd, P0, P1),
	  (Cmd = ascIO,
           P0 = program(ops, H, Rings, memory(Index, AVL0), P),
	   get_active_ring(ops, Rings, ring(ops, _, _, value(0))),
	   !,
	   get(Char),
	   replace_node(cell(Index, _), cell(Index, value(Char)), AVL0, AVL1),
	   P1 = program(ops, H, Rings, memory(Index, AVL1), P))).

:- aspect(around, command(_, _, _), prove_goal).
