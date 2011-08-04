% file:     whirl_interpreter.pl
% link:     http://www.bigzaphod.org/whirl
% date:     November 15, 2005
% author:   Douglas M. Auclair (DMA)
% version:  0.01
% synopsis: Interprets a whirl program (input as a string).  As Kang
%           Seonghoon's C version weighs in at only 16 lines of code, I don't
%           think it would be appropriate for this interpreter to go for
%           brevity, but it will have some other interesting features such as a
%           program tracing facility (step-by-step or as debugging output),
%           and, if I'm feeing saucy, a "best path to instruction Wheel:X"
%           facility (that /will/ require some moxy, and a larger version
%           number).

% whirl/1 takes an input whirl program (a string) and interprets the
% instructions of that program.

% uncomment next line for Quintus-compatability
% :- ensure_loaded(library(math)).

whirl([Whirl|Instructions]) :-
  initialize_ops_ring(Ops),
  initialize_math_ring(Math),
  initialize_memory(Memory),
  initialize_program([Whirl|Instructions], 0, Program, []),
  interpret(program(ops, history(quescient, last_instruction(1)),
                    [Ops, Math], Memory, program(Program)), _).
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% interpreter implementation

interpret -->
  instruction(X),
  execute(X),
  interpret_next_instruction.

instruction(Bit, P, P) :-
%    grabs the next instruction of the program IR
  P = program(_, _, _, _, program([_ - Bit|_])).

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
  get_command(Cmd),
  command(Cmd),
  switch_wheel,
  history_is(executed, 0).
execute_command -->
%    ... otherwise move along.
  history_is(quescient, 0).

interpret_next_instruction(P0, P) :-
%    Loops until we run out of tokens to interpret
  P0 = program(Active, Hist, Rings, Mem, program([Idx - Op|Codes])),
  NewIndex is Idx + 1,
  (takeout(NewIndex - Code, [Idx - Op|Codes], Program) ->
     interpret(program(Active, Hist, Rings, Mem,
                       program([NewIndex - Code|Program])), P)
   ;
     P = P0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% state transformation helper predicates

rotate(program(Active, Hist, Rings, Mem, P),
       program(Active, Hist, [NewRing|Others], Mem, P)) :-
%    A '1' token: spin current wheel in current direction
  takeout(ring(Active, [Idx-Cmd|Insts], Spin, Val), Rings, Others),
  direction(Spin, Offset),
  SemiIdx is Idx + Offset,
  adjust(SemiIdx, NewIdx),
  takeout(NewIdx-NewCmd, [Idx-Cmd|Insts], NewInsts),
  NewRing = ring(Active, [NewIdx-NewCmd|NewInsts], Spin, Val).

history_is(State, Inst, program(Active, _, Rings, Mem, P),
           program(Active, History, Rings, Mem, P)) :-
  History = history(State, last_instruction(Inst)).

reverse_spin(program(Active, Hist, Rings, Mem, P),
	     program(Active, Hist, [ActiveRing|Passes], Mem, P)) :-
%    A '0' token: at the least we will reverse the direction of this wheel
  takeout(ring(Active, Insts, Spin, Val), Rings, Passes),
  spin_reverse(Spin, NewSpin),
  ActiveRing = ring(Active, Insts, NewSpin, Val).

spin_reverse(clockwise spin, counterclockwise spin).
spin_reverse(counterclockwise spin, clockwise spin).

direction(clockwise spin, 1).
direction(counterclockwise spin, -1).

history(State, Inst, P, P) :-
  P = program(_, history(State, last_instruction(Inst)), _, _, _).

get_command(Command, P, P) :-
%    Convert this token to an equivalent command by looking it up on
%    the active wheel
  P = program(Active, _, Rings, _, _),
  ring(Active, [_-Command|_], _, _) <- Rings.

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
command(zero, program(Active, Hist, Rings, M, P),
	program(Active, Hist, [ring(Active, I, Spin, value(0))|Ring], M, P)) :-
  takeout(ring(Active, I, Spin, _), Rings, Ring).
command(load, program(Active, Hist, Rings, Mem, P),
        program(Active, Hist, [ring(Active, I, Spin, Value)|Ring], Mem, P)) :-
  Mem = memory([cell(_, Value)|_]),
  takeout(ring(Active, I, Spin, _), Rings, Ring).
command(store, program(Active, Hist, Rings, memory([cell(Idx, _)|Cells]), P),
        program(Active, Hist, Rings, memory([cell(Idx, Value)|Cells]), P)) :-
  ring(Active, _, _, Value) <- Rings.

% ops-specific commands:

command(exit, _, program(_, _, _, _, program([]))).
command(one, program(ops, Hist, Rings, Mem, P),
	program(ops, Hist, [ring(ops, I, Spin, value(1))|Maths], Mem, P)) :-
  takeout(ring(ops, I, Spin, _), Rings, Maths).
command(padd, program(ops, Hist, Rings, Mem, program([Idx - Opcode|Opcodes])),
        program(ops, Hist, Rings, Mem, program([NewerIdx - Op|Codes]))) :-
%    Jumps to program address X, if it exists; SEGVs sinon
  ring(ops, _, _, value(X)) <- Rings,
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
  ring(ops, _, _, value(X)) <- Rings,
  floor(X, Y),
  NewIdx is Y + Idx,
  (takeout(cell(NewIdx, SomeNewV), Memory, SomeMem) ->
     NewV = SomeNewV,
     Mem = SomeMem
   ;
     NewV = 0,
     Mem = Memory).

% Okay, it would be an interesting coincidence to use the 'logic' opcode,
% but I suppose optimizing compilers would select this command in favor of 
% one of the other brute-force assignment opcodes ...

command(logic, program(ops, Hist, Rings, Mem, P),
        program(ops, Hist, [ring(ops, Inst, Spin, value(0))|Maths], Mem, P)) :-
  Mem = memory([cell(_, value(0))|_]),
  !,
  takeout(ring(ops, Inst, Spin, _), Rings, Maths).
command(logic, program(ops, Hist, Rings, Mem, P),
        program(ops, Hist, [ring(ops, Inst, Spin, value(Val))|Maths],
                Mem, P)) :-
  takeout(ring(ops, Inst, Spin, value(X)), Rings, Maths),
  floor(X, Y),
  Val is Y /\ 1.

% 'if' as in 'jump-not-zero' -- the OISC has only one opcode:
% 'decrement-jump-not-zero'; can you believe that OISC is Turing-equivalent?

command(if, P, P) :-
  P = program(ops, _, _, memory([cell(_, value(0))|_]), _),
  !.
command(if) --> command(padd).
command(intIO, program(ops, Hist, Rings, memory([cell(Idx, _)|Cells]), P),
        program(ops, Hist, Rings, memory([cell(Idx, Num)|Cells]), P)) :-
  ring(ops, _, _, value(0)) <- Rings,
  !,
  read_term(Num, []),
  (integer(Num) -> true; raise_exception(integral_expected(received(Num)))).
command(intIO, P, P) :-
  P = program(ops, _, _, memory([cell(_, value(V))|_]), _),
  floor(V, Y),
  write(Y).
command(ascIO, program(ops, Hist, Rings, memory([cell(Idx, _)|Cells]), P),
        program(ops, Hist, Rings, memory([cell(Idx, Char)|Cells]), P)) :-
  ring(ops, _, _, value(0)) <- Rings,
  !,
  get(Char).
command(ascIO, P, P) :-
  P = program(ops, _, _, memory([cell(_, value(V))|_]), _),
  floor(V, Y),
  put(Y).

% math-specific commands: these commands turned out to be very easy to
% implement given Prolog's 'program-as-proof' methodology

command(MathOp, program(math, Hist, Rings, M, P),
        program(math, Hist, [ring(math, I, S, value(Value))|Ops], M, P)) :-
  MathOp <- [add, mult, div, neg],
  takeout(ring(math, I, S, value(A)), Rings, Ops),
  M = memory([cell(_, value(B))|_]),
  arithmetic(MathOp, A, B, Value).
command(CmpOp, program(math, Hist, Rings, M, P),
        program(math, Hist, [ring(math, I, S, value(Value))|Ops], M, P)) :-
  CmpOp <- ['<', '>', '=', not],
  takeout(ring(math, I, S, value(A)), Rings, Ops),
  M = memory([cell(_, value(B))|_]),
  comparison(CmpOp, A, B, Value).

arithmetic(add, A, B, Value) :- Value is A + B.
arithmetic(mult, A, B, Value) :- Value is A * B.
arithmetic(div, A, B, Value) :- Value is A / B.
arithmetic(neg, A, _, Value) :- Value is A * -1.

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

Elt <- List :- takeout(Elt, List, _).
