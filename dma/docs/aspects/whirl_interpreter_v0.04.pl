% file:     whirl_interpreter.pl
% link:     http://www.bigzaphod.org/whirl
% date:     November 15, 2005
% author:   Douglas M. Auclair (DMA)
% version:  0.04
% synopsis: Interprets a whirl program (input as a string).  As Kang
%           Seonghoon's C version weighs in at only 16 lines of code,
%           I don't think it would be appropriate for this interpreter
%           to go for brevity, but it will have some other interesting
%           features such as a program tracing facility (step-by-step
%           or as debugging output), and, if I'm feeing saucy, a "best
%           path to instruction Wheel:X" facility (that /will/ require
%           some moxy, and a larger version number).

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

:- ensure_loaded(library(math)).

% whirl/1 takes an input whirl program (a string) and interprets the
% instructions of that program.

whirl([Whirl|Insts]) :-
  initialize_ops_ring(Ops),
  initialize_math_ring(Math),
  initialize_memory(Memory),
  initialize_program([Whirl|Insts], 0, Opcodes, []),
  build_program_state(ops, history(quescient, last_instruction(1)),
		      Ops, Math, Memory, Opcodes, Program),
  interpret(Program, _).
whirl([]).
%    The empty program string is a noop

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% initialization predicates

% un peu de syntax to make wheel direction more declarative

:- op(200, yf, spin).
:- op(200, xfy, '<-').

initialize_ops_ring(ring(ops, Instructions, clockwise spin, value(0))) :-
  Instructions = [0-noop, 1-exit, 2-one, 3-zero, 4-load, 5-store,
		  6-padd, 7-dadd, 8-logic, 9-if, 10-intIO, 11-ascIO].

initialize_math_ring(ring(math, Instructions, clockwise spin, value(0))) :-
  Instructions = [0-noop, 1-load, 2-store, 3-add, 4-mult, 5-div,
		  6-zero, 7-'<', 8-'>', 9-'=', 10-not, 11-neg].

initialize_memory(memory([cell(0, value(0))])).
%    Memory is allowed to be of infinite size; we handle the actual size
%    needed for the memory lazily.

initialize_program([Op|Codes], Index) -->
  ({ Code is Op - 48,
     Code <- [0, 1] } ->
       [Index - Code],
       { NewIndex is Index + 1 }
     ;
       { NewIndex = Index }),
  initialize_program(Codes, NewIndex).
initialize_program([], _) --> [-1 - error].

build_program_state(Active, Hist, Ops, Math, Mem, Opcodes,
		    program(Active, Hist, [Ops, Math], Mem, program(Opcodes))).

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
  P0 = program(Active, Hist, Rings, Mem, program([Idx - Op|Codes])),
  NewIndex is Idx + 1,
  takeout(NewIndex - Code, [Idx - Op|Codes], Program),
  P1 = program(Active, Hist, Rings, Mem, program([NewIndex - Code|Program])).

instruction(Bit, P, P) :-
%    grabs the next instruction of the program IR
  P = program(_, _, _, _, program([_ - Bit|_])).

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

get_active_ring(Active, Rings, ring(Active, Insts, Spin, Val)) :-
  ring(Active, Insts, Spin, Val) <- Rings.

update_ring(ring(Type, Insts, Spin, Val), Rings, 
            [ring(Type, Insts, Spin, Val), OtherRing]) :-
  takeout(ring(Type, _, _, _), Rings, [OtherRing]).

get_command(ring(_, Insts, _, _), Idx, Cmd) :-
  (Idx - Cmd) <- Insts.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% state transformation helper predicates

rotate(program(Active, Hist, Rings0, Mem, P),
       program(Active, Hist, Rings1, Mem, P)) :-
%    A '1' token: spin current wheel in current direction
  get_active_ring(Active, Rings0, Ring),
  Ring = ring(Active, [Idx-Cmd|Cmds], Spin, Val),
  direction(Spin, Offset),
  SemiIdx is Idx + Offset,
  adjust(SemiIdx, NewIdx),
  get_command(Ring, NewIdx, NewCmd),
  takeout(NewIdx-NewCmd, [Idx-Cmd|Cmds], NewCmds),
  update_ring(ring(Active, [NewIdx-NewCmd|NewCmds], Spin, Val), Rings0, Rings1).

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

direction(clockwise spin, 1).
direction(counterclockwise spin, -1).

history(State, Inst, P, P) :-
  P = program(_, history(State, last_instruction(Inst)), _, _, _).

get_selected_command(Command, P, P) :-
%    Convert this token to an equivalent command by looking it up on
%    the active wheel
  P = program(Active, _, Rings, _, _),
  get_active_ring(Active, Rings, ring(_, [_-Command|_], _, _)).

switch_wheel(program(Prev, H, R, M, P), program(New, H, R, M, P)) :-
  switcheroo(Prev, New).

switcheroo(ops, math).
switcheroo(math, ops).

adjust(A, B) :-
%    Ensure we're always [0,11] (inclusive) for the current wheel slot
  floor(A, Floored),
  Temp is Floored mod 12,
  (Temp < 0 -> B = 11; B = Temp).

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

command(exit, _, program(_, _, _, _, program([]))).
command(one) --> update_active_ring_value(1).
command(padd, program(ops, Hist, Rings, Mem, program([Idx - Opcode|Opcodes])),
        program(ops, Hist, Rings, Mem, program([NewerIdx - Op|Codes]))) :-
%    Jumps to program address X, if it exists; SEGVs sinon
  get_active_ring(ops, Rings, ring(ops, _, _, value(X))),
  floor(X, Y),
  NewIdx is Idx + Y,
  (takeout(NewIdx - _, [Idx - Opcode|Opcodes], _) ->
     NewerIdx is NewIdx - 1,
     takeout(NewerIdx - Op, [Idx - Opcode|Opcodes], Codes)
   ;
     raise_exception(sigsegv(no_address(NewIdx), from(Idx)))).
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% list utilities use by the interpreter

takeout(H, [H|T], T). 
takeout(Elt, [H|T], [H|R]) :-
  takeout(Elt, T, R).

Elt <- [Elt|_].
Elt <- [_|Rest] :- Elt <- Rest.
