:- use_module(library(aspects), [aspect/3]).

:- aspect(after, initialize_memory(_),
	  (tell('the_whirl_program.pl'),
	   write('% auto-generated: instruction/2 contains the whirl program'),
           nl, nl)).

:- aspect(before, build_program_state(_, _, _, _, _, _, _),
          (told, compile(the_whirl_program))).

:- aspect(around, push_instruction(Index, Opcode, A, B),
	  (format('instruction(~d, ~w).~n', [Index, Opcode]), A = B)).

:- aspect(around, build_program_state(A, H, Ops, Math, Mem, _Opcodes, Program),
          (Program = program(A, H, rings(Ops, Math), Mem, instruction_index(0)),
           prove_goal(_, _, _, _, _, _, _))).

:- aspect(around, bump_program_counter(P0, P1),
          (P0 = program(A, H, R, M, instruction_index(Index)),
           NewIndex is Index + 1,
           P1 = program(A, H, R, M, instruction_index(NewIndex)))).

:- dynamic instruction/2.
:- aspect_declaration(dynamic instruction/2).

:- aspect(around, instruction(Instruction, P, P),
          (P = program(_, _, _, _, instruction_index(Index)),
           instruction(Index, Instruction))).

:- aspect(around, command(Command, _P0, P1),
          (Command = exit,
           !,
           P1 = program(_, _, _, _, instruction_index(-999)))).

:- ensure_loaded(library(math)).
:- aspect_declaration(ensure_loaded(library(math))).

:- aspect(around, command(Command, P0, P1),
          (Command = padd,
           !,
           P0 = program(ops, H, Rings, M, instruction_index(Index)),
           get_active_ring(ops, Rings, ring(_, _, _, value(X))),
           floor(X, Y),
           NewIndex is Index + Y,
           (instruction(NewIndex, _) -> true;
                raise_exception(sigsegv(no_address(NewIndex), from(Index)))),
	   NewerIndex is NewIndex - 1,
           P1 = program(ops, H, Rings, M, instruction_index(NewerIndex)))).

:- aspect(around, command(_, _, _), prove_goal).
