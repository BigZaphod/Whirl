:- use_module(library(aspects), [aspect/3, aspect_declaration/1]).

:- use_module(library(command_utils), [rotate_in_direction/3]).
:- aspect_declaration(use_module(library(command_utils), [rotate_in_direction/3])).

:- op(200, xfx, ::).
:- aspect_declaration(op(200, xfx, ::)).

:- op(200, yf, spin).
:- aspect_declaration(op(200, yf, spin)).

:- aspect(around, initialize_ops_ring(Ring),
	      Ring = ring(ops, ops::noop, clockwise spin, value(0))).
:- aspect(around, initialize_math_ring(Ring),
	      Ring = ring(math, math::noop, clockwise spin, value(0))).

/* get command is now superfluous
 * :- aspect(around, get_command(Ring, Index, Command),
 *           Ring = ring(Type, Type:Command, _, _)).
 */

:- aspect(around, rotate(P0, P1),
          (P0 = program(Active, Hist, Rings0, Mem, Index),
		   P1 = program(Active, Hist, Rings1, Mem, Index),
		   get_active_ring(Active, Rings0, ring(_, Active::Cmd, Spin, Val)),
		   rotate_in_direction(Spin, Active::Cmd, NewCmd),
		   update_ring(ring(Active, NewCmd, Spin, Val), Rings0, Rings1))).

:- aspect(around, get_selected_command(Cmd, P0, P1),
		  (P0 = program(Active, _, Rings, _, _),
		   P1 = P0,
		   get_active_ring(Active, Rings, ring(_, Active::Cmd, _, _)))).
