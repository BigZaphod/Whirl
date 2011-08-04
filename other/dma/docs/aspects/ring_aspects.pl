:- use_module(library(aspects), [aspect/3]).

:- aspect(around, build_program_state(A, H, Ops, Math, Mem, Opcodes, Program),
          Program = program(A, H, rings(Ops, Math), Mem, program(Opcodes))).

:- aspect(around, get_active_ring(A, Rs, Ring), 
	  aspects_utils:get_active_ring_proxy(A, Rs, Ring)).

:- aspect(around, update_ring(Ring, Old, New),
	  aspect_utils:update_ring_proxy(Ring, Old, New)).


