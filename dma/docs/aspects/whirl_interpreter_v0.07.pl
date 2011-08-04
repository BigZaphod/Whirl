% file:     whirl_interpreter.pl
% link:     http://www.bigzaphod.org/whirl
% date:     November 15, 2005
% author:   Douglas M. Auclair (DMA)
% version:  0.07
% synopsis: Interprets a whirl program (input as a string).  As Kang
%           Seonghoon's C version weighs in at only 16 lines of code,
%           I don't think it would be appropriate for this interpreter
%           to go for brevity, but it will have some other interesting
%           features such as a program tracing facility (step-by-step
%           or as debugging output), and, if I'm feeing saucy, a "best
%           path to instruction Wheel:X" facility (that /will/ require
%           some moxy, and a larger version number).

% modified: December 12, 2005, DMA
% changed:  Externalized command selection to the command_utils module and
%           discarded or changed predicates interacting with ring commands.

% modified: December 11, 2005, DMA
% changed:  The prototyped optimization of converting the instructions into
%           a set of facts worked very well (100 times faster than the
%           previous version), so this version rolls those changes into the
%           source code.

% modified: December 10, 2005, DMA
% changed:  Rolled aspect prototype for ring access into this version.
%           Before the rings were stored in a list; now they are arguments
%           to the typed rings/2 term.  Also externalized program instruction
%           scanning so that aspects may modify how instructions are stored
%           without affecting other parts of scanning.

% modified: December 9, 2005, DMA
% changed:  Added a build_program_state/7 predicate to allow aspects
%           to control what the program state looks like.  Converted
%           all ring selection operations to the standard get_active_ring/3
%           predicate call.

% modified: December 8, 2005, DMA
% changed:  Added predicates to specify how we are using the majorly-used
%           predicate (takeout/2), as it fetches information in several
%           different roles.

% modified: November 22, 2005, DMA
% changed:  Debugging shows that memory cells where being
%           improperly reset to the integral 0, instead of the term,
%           value(0); fixed this error.  A better typing system, such
%           as the Hindley-Milner types found in Haskell
%           (http://haskell.org) and Mercury
%           (http://cs.mu.oz.au/research/mercury) would have revealed
%           this issue right away, but we work with tools available...

:- use_module(library(list_utils), [(<-)/2, takeout/3]).
:- use_module(library(command_utils), [rotate_in_direction/3]).
:- ensure_loaded(library(math)).

% whirl/1 takes an input whirl program (a string) and interprets the
% instructions of that program.

whirl([Whirl|Insts]) :-
  initialize_ring(ops, Ops),
  initialize_ring(math, Math),
  initialize_memory(Memory),
  tell('the_whirl_program.pl'),
  format('% auto-generated: instruction/2 contains the whirl program~2n', []),
  initialize_program([Whirl|Insts], 0),
  told,
  compile(the_whirl_program),
  build_program_state(ops, history(quescient, last_instruction(1)),
		      Ops, Math, Memory, Program),
  interpret(Program, _).
whirl([]).
%    The empty program string is a noop

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% initialization predicates

% un peu de syntax to make wheel direction more declarative

:- op(200, yf, spin).
:- op(200, xfx, ::).

initialize_ring(Type, ring(Type, Type::noop, clockwise spin, value(0))).

initialize_memory(memory([cell(0, value(0))])).
%    Memory is allowed to be of infinite size; we handle the actual size
%    needed for the memory lazily.

initialize_program([Op|Codes], Index) :-
  (Code is Op - 48,
   Code <- [0, 1] ->
     push_instruction(Index, Code),
     NewIndex is Index + 1
   ;
     NewIndex = Index),
  initialize_program(Codes, NewIndex).
initialize_program([], _) :- push_instruction(-1, error).

push_instruction(Index, Instruction) :-
  format('instruction(~d, ~w).~n', [Index, Instruction]).

build_program_state(Active, Hist, Ops, Math, Mem,
		    program(Active, Hist, rings(Ops, Math), Mem,
		            instruction_index(0))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% interpreter implementation

interpret -->
  instruction(X),
  execute(X),
  bump_program_counter,
  !,
  interpret.
interpret --> [].

% The execution model is rather complicated (read: stateful):
%   the 1 opcode is always rotate, but 0 changes wheel direction
%   and then possibly executes an instruction, depending on what
%   the last instruction was and what it did.  That's why we have
%   an auxilary predicate to aid execute/3.

execute(1) -->
  rotate,
  history_is(quescient, 1).
execute(0) -->
  reverse_spin,
  execute_command.

execute_command -->
%    Given the last instruction was 0 and did nothing, do all the below ...
  history(quescient, 0),
  !,
  get_selected_command(Cmd),
  command(Cmd),
  switch_wheel,
  history_is(executed, 0).
execute_command -->
%    ... otherwise move along.
  history_is(quescient, 0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% data access predicates

bump_program_counter(P0, P1) :-
  P0 = program(Active, Hist, Rings, Mem, instruction_index(Idx)),
  NewIndex is Idx + 1,
  P1 = program(Active, Hist, Rings, Mem, instruction_index(NewIndex)).

:- dynamic instruction/2.

instruction(Instruction, P, P) :-
%    grabs the next instruction of the program IR
  P = program(_, _, _, _, instruction_index(Index)),
  instruction(Index, Instruction).

update_active_ring_value(Val, program(Active, Hist, Rings0, Mem, Prog),
			 program(Active, Hist, Rings1, Mem, Prog)) :-
  get_active_ring(Active, Rings0, ring(_, I, S, _)),
  update_ring(ring(Active, I, S, value(Val)), Rings0, Rings1).

change_active_ring_value_by(_Seed, Accum, Function, Output,
			    program(Active, Hist, Rings0, Mem, Prog),
			    program(Active, Hist, Rings1, Mem, Prog)) :-
  get_active_ring(Active, Rings0, ring(_, I, S, value(Accum))),
  Function,
  update_ring(ring(Active, I, S, value(Output)), Rings0, Rings1).

selected_memory_cell(Val, program(A, H, R, Memory, Prog),
		     program(A, H, R, Memory, Prog)) :-
  Memory = memory([cell(_, value(Val))|_]).

get_active_ring(ops, rings(Ops, _), Ops).
get_active_ring(math, rings(_, Math), Math).

update_ring(ring(ops, Insts, Spin, Val), rings(_, Math), 
            rings(ring(ops, Insts, Spin, Val), Math)).
update_ring(ring(math, Insts, Spin, Val), rings(Ops, _), 
            rings(Ops, ring(math, Insts, Spin, Val))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% state transformation helper predicates

rotate(program(Active, Hist, Rings0, Mem, P),
       program(Active, Hist, Rings1, Mem, P)) :-
%    A '1' token: spin current wheel in current direction
  get_active_ring(Active, Rings0, ring(_, Active::Cmd, Spin, Val)),
  rotate_in_direction(Spin, Active::Cmd, NewCmd),
  update_ring(ring(Active, NewCmd, Spin, Val), Rings0, Rings1).

history_is(State, Inst, program(Active, _, Rings, Mem, P),
           program(Active, History, Rings, Mem, P)) :-
  History = history(State, last_instruction(Inst)).

reverse_spin(program(Active, Hist, Rings0, Mem, P),
	     program(Active, Hist, Rings1, Mem, P)) :-
%    A '0' token: at the least we will reverse the direction of this wheel
  get_active_ring(Active, Rings0, ring(Active, Insts, Spin, Val)),
  spin_reverse(Spin, NewSpin),
  update_ring(ring(Active, Insts, NewSpin, Val), Rings0, Rings1).

spin_reverse(clockwise spin, counterclockwise spin).
spin_reverse(counterclockwise spin, clockwise spin).

history(State, Inst, P, P) :-
  P = program(_, history(State, last_instruction(Inst)), _, _, _).

get_selected_command(Command, P, P) :-
%    Convert this token to an equivalent command by looking it up on
%    the active wheel
  P = program(Active, _, Rings, _, _),
  get_active_ring(Active, Rings, ring(_, Active::Command, _, _)).

switch_wheel(program(Prev, H, R, M, P), program(New, H, R, M, P)) :-
  switcheroo(Prev, New).

switcheroo(ops, math).
switcheroo(math, ops).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% commands

% general commands -- these work on either ring:

command(noop) --> [].
command(zero) --> update_active_ring_value(0).
command(load) --> selected_memory_cell(Val), update_active_ring_value(Val).
command(store, program(Active, Hist, Rings, memory([cell(Idx, _)|Cells]), P),
        program(Active, Hist, Rings, memory([cell(Idx, Value)|Cells]), P)) :-
  get_active_ring(Active, Rings, ring(_, _, _, Value)).

% ops-specific commands:

command(exit, _, program(_, _, _, _, instruction_index(-999))).
command(one) --> update_active_ring_value(1).
command(padd, program(ops, Hist, Rings, Mem, instruction_index(Index)),
        program(ops, Hist, Rings, Mem, instruction_index(NewerIndex))) :-
%    Jumps to program address X, if it exists; SEGVs sinon
  get_active_ring(ops, Rings, ring(ops, _, _, value(X))),
  floor(X, Y),
  NewIndex is Index + Y,
  (instruction(NewIndex, _) -> true;
    raise_exception(sigsegv(no_address(NewIndex), from(Index)))),
  NewerIndex is NewIndex - 1.
command(dadd, program(ops, Hist, Rings, memory(Memory), P),
        program(ops, Hist, Rings, memory([cell(NewIdx, NewV)|Mem]), P)) :-
%    Here we jump to a new memory address, given that that cell exists.
%    If the cell doesn't exist, create it and add it to the memory store;
%    thereby "lazily" growing the memory store as needed.
%
%    Remember, it's not a memory leak, it's lazy growth of the memory store
%    (not in any respect resembling a memory leak ... *cough*).
  Memory = [cell(Idx, _)|_],
  get_active_ring(ops, Rings, ring(ops, _, _, value(X))),
  floor(X, Y),
  NewIdx is Y + Idx,
  (takeout(cell(NewIdx, SomeNewV), Memory, SomeMem) ->
     NewV = SomeNewV,
     Mem = SomeMem
   ;
     NewV = value(0),
     Mem = Memory).

% Okay, it would be an interesting coincidence to use the 'logic' opcode,
% but I suppose optimizing compilers would select this command in favor of 
% one of the other brute-force assignment opcodes ...

command(logic) --> selected_memory_cell(0), !, update_active_ring_value(0).
command(logic) -->
  change_active_ring_value_by(_, Accum, and1(Accum, Ans), Ans).

% 'if' as in 'jump-not-zero' -- the OISC has only one opcode:
% 'decrement-jump-not-zero'; can you believe that OISC is Turing-equivalent?

command(if) --> selected_memory_cell(0), !.
command(if) --> command(padd).
command(intIO, program(ops, Hist, Rings, memory([cell(Idx, _)|Cells]), P),
        program(ops, Hist, Rings, memory([cell(Idx, value(N))|Cells]), P)) :-
  get_active_ring(ops, Rings, ring(ops, _, _, value(0))),
  !,
  read_term(N, []),
  (integer(N) -> true; raise_exception(integral_expected(received(N)))).
command(intIO) --> selected_memory_cell(V), { floor(V, Y), write(Y) }.
command(ascIO, program(ops, Hist, Rings, memory([cell(Idx, _)|Cells]), P),
        program(ops, Hist, Rings, memory([cell(Idx, value(C))|Cells]), P)) :-
  get_active_ring(ops, Rings, ring(ops, _, _, value(0))),
  !,
  get(C).
command(ascIO) --> selected_memory_cell(V), { floor(V, Y), put(Y) }.

% math-specific commands: these commands turned out to be very easy to
% implement given Prolog's 'program-as-proof' methodology

:- op(200, xfy, <-).

command(MathOp) -->
  { MathOp <- [add, mult, div, neg] },
  selected_memory_cell(B),
  change_active_ring_value_by(B, Accum, arithmetic(MathOp, Accum, B, Ans), Ans).
command(CmpOp) -->
  { CmpOp <- ['<', '>', '=', not] },
  selected_memory_cell(B),
  change_active_ring_value_by(B, Accum, comparison(CmpOp, Accum, B, Ans), Ans).

arithmetic(add, A, B, Value) :- Value is A + B.
arithmetic(mult, A, B, Value) :- Value is A * B.
arithmetic(div, A, B, Value) :- Value is A / B.
arithmetic(neg, A, _, Value) :- Value is A * -1.

and1(Accum, Ans) :- floor(Accum, X), Ans is X /\ 1.

comparison('<', A, B, 1) :- A < B.
comparison('>', A, B, 1) :- A > B.
comparison('=', A, A, 1).
comparison(not, 0, _, 1).
comparison(_, _, _, 0).
