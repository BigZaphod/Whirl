% module:   ring_utils
% date:     December 9, 2005
% author:   Douglas M. Auclair (DMA)
% synposis: The replacement code the aspect calls to support the changed
%           data layout of the program state for the rings data structure.

:- module(rings_utils, [get_active_ring_proxy/3, update_ring_proxy/3]).

get_active_ring_proxy(ops, rings(Ops, _), Ops).
get_active_ring_proxy(math, rings(_, Math), Math).

update_ring_proxy(ring(ops, I, S, V), rings(_, Math),
		  rings(ring(ops, I, S, V), Math)).
update_ring_proxy(ring(math, I, S, V), rings(Ops, _),
		  rings(Ops, ring(math, I, S, V))).
