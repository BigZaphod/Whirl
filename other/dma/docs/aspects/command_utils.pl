% module:   command_utils
% date:     December 11, 2005
% author:   Douglas M. Auclair (DMA)
% synopsis: Expresses the neighbor relation of the commands on each ring
% history:  Up to this point, commands were indexed and stored in an unordered
%           list, with the unfortunate side-effect of making access time O(N),
%           and also adding to the burden of housekeeping with index boundary
%           management.  The solution below is simpler, smaller and faster.

:- module(command_utils, [rotate_in_direction/3]).

:- op(200, yf, spin).

rotate_in_direction(clockwise spin) --> clockwise_command.
rotate_in_direction(counterclockwise spin) --> counterclockwise_command.

clockwise_command --> command.
counterclockwise_command(A, B) :- command(B, A).

:- op(200, xfx, ::).

% the circle of ops commands:
command(ops::noop, ops::exit).
command(ops::exit, ops::one).
command(ops::one, ops::zero).
command(ops::zero, ops::load).
command(ops::load, ops::store).
command(ops::store, ops::padd).
command(ops::padd, ops::dadd).
command(ops::dadd, ops::logic).
command(ops::logic, ops::if).
command(ops::if, ops::intIO).
command(ops::intIO, ops::ascIO).
command(ops::ascIO, ops::noop).

% the circle of math commands:
command(math::noop, math::load).
command(math::load, math::store).
command(math::store, math::add).
command(math::add, math::mult).
command(math::mult, math::div).
command(math::div, math::zero).
command(math::zero, math::'<').
command(math::'<', math::'>').
command(math::'>', math::'=').
command(math::'=', math::not).
command(math::not, math::neg).
command(math::neg, math::noop).
