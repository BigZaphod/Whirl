:- use_module(library(aspects), [aspect/3, aspect_declaration/1]).

:- dynamic memory/2.
:- aspect_declaration(dynamic memory/2).

:- ensure_loaded(library(math)).
:- aspect_declaration(ensure_loaded(library(math))).

:- aspect(around, initialize_memory(Mem),
	      (Mem = memory_index(0),
		   retractall(memory(_, _)),
		   assert(memory(0, value(0))))).

:- aspect(around, selected_memory_cell(Val, P0, P1),
          (P0 = program(_, _, _, memory_index(X), _),
		   memory(X, value(Val)),
		   P1 = P0)).

:- aspect(around, command(Cmd, P0, P1),
		  (Cmd = store,
		   !,
		   P0 = program(A, _, R, memory_index(X), _),
		   P1 = P0,
		   retract(memory(X, _)),
		   get_active_ring(A, R, ring(_, _, _, Value)),
		   assert(memory(X, Value)))).
:- aspect(around, command(Cmd, P0, P1),
		  (Cmd = dadd,
		   !,
		   P0 = program(ops, H, R, memory_index(Index), P),
		   get_active_ring(ops, R, ring(ops, _, _, value(X))),
		   floor(X, Y),
		   NewIndex is Y + Index,
		   (memory(NewIndex, _) -> true; assert(memory(NewIndex, value(0)))),
		   P1 = program(ops, H, R, memory_index(NewIndex), P))).
:- aspect(around, command(Cmd, P0, P1),
		  (Cmd = intIO,
		   P0 = program(ops, _, R, memory_index(Index), _),
		   P1 = P0,
		   get_active_ring(ops, R, ring(ops, _, _, value(0))),
		   !,
	  	   read_term(N, []),
		   (integer(N) -> true; raise_exception(integral_expected(received(N)))),
		   retract(memory(Index, _)),
		   assert(memory(Index, value(N))))).
:- aspect(around, command(Cmd, P0, P1),
		  (Cmd = ascIO,
		   P0 = program(ops, _, R, memory_index(Index), _),
		   P1 = P0,
		   get_active_ring(ops, R, ring(ops, _, _, value(0))),
		   !,
		   get(C),
		   retract(memory(Index, _)),
		   assert(memory(Index, value(C))))).
:- aspect(around, command(_, _, _), prove_goal).