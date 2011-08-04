:- use_module(library(aspects), [aspect/3, aspect_declaration/1]).

:- use_module(library(list_utils), [takeout/3]).
:- aspect_declaration(use_module(library(list_utils), [takeout/3])).
:- ensure_loaded(library(lists)).
:- aspect_declaration(ensure_loaded(library(lists))).

:- aspect(around, list_utils:takeout(Elt, L0, L1), delete(L0, Elt, L1)).