% module:   aspects
% date:     November 2, 2005, All Souls' Day
% author:   Douglas M. Auclair (DMA)

% copyright (c) 2005, Cotillion Group, Inc.  All rights reserved.

% synopsis: Allows the user to modify behaviors of predicates.  It is
%           best to declare aspects at the top-level (module(user))
%           and in immediate mode (':- aspect(...)').
%
%           Once the aspects are declared, build the qofs of the
%           program with aspects by declaring an aspect namespace with
%           program(Namespace). To run the program without aspects, all
%           aspect-affected qof must be recompiled.

:- module(aspects, [program/1, aspect/3, aspect_declaration/1]).

% modified: December 11, 2005, DMA
% changed:  Added recursive descent to print_body/6 (by adding an
%           auxiliary predicate print_body_aux/6) to handle, properly,
%           embedded prove_goal/[0, N] special forms.

% modified: December 4, 2005, DMA
% changed:  Added a carriage return to separate declarations in
%           aspect-generated files.

% modified: December 2, 2005, DMA
% changed:  Added comments throughout; replaced takeout/3 calls with
%           (<-)/2 where appropriate; allowed for multiple before
%           and after aspects as well as multiple around aspects.

% modified: November 8, 2005, DMA
% changed:  Added conditional control for top-level goals

% modified: November 7, 2005, DMA
% changed:  Added term expansion for definite clauses as this system
%           did not expand clauses in definite form.

% modified: November 5, 2005, DMA
% changed:  We've proved that aspects work with a meta-interpreter and
%           dynamic predicates, and have prototyped a system that
%           compiles aspects inline.  This shows the AOP-effort has a
%           two-fold direction.  This file represents the
%           compiling-aspects effort; the file in the sybling
%           'interpreted/' directory represents the other branch of
%           work.

% modified: November 3, 2005, DMA
% changed:  Added a dynamic fact-table that tracks which predicates
%           have declared aspects.  This facilitates the work that
%           needs to be done for a system that inlines and compiles
%           aspects into a static program model.  We move the actual
%           defining aspects to an auxilary predicate where the main
%           predicate asserts the aspect declaration into the database
%           dynamically.

:- use_module(library(syntax), [(<-)/2]).
:- use_module(library(list_utils), [takeout/3, filter/4, map/3]).

:- ensure_loaded([library(date), library(strings), library(print_chars)]).

:- op(200,  yfx, <-).

% aspect/3 -- the execution model for predicates that have aspects
% defined is as follows:
%
%    1. around(Pred)
%  and possibly
%    2. opt(before(Pred)), Pred, opt(after(Pred))
%

aspect(Type, Callable, Block) :-
%    Creates an aspect of type Type on Callable and stores a marker of
%    the aspect into the database.
  Type <- [around, before, after],
  (Callable = Mod:Goal ->
             Call = Goal, Module = Mod
           ; Call = Callable, Module = user),
  assert(aspects:aspect_def(Type, Module:Call, Block)).

% aspect_declaration/1 -- provides a facility to put declarations,
% such as op/3 declarations into the aspect proxy file.

aspect_declaration(Declaration) :-
%    Create a declaration to be inserted into the aspect proxy file
  assert(aspects:aspect_decl(Declaration)).

program(Module) :-
%    Creates module Module: program/1 takes the collected aspect
%    declarations and converts them into compilable predicates of
%    Module.
  harvest_aspects(Aspects),
  time_stamp('%M %d, %y', Date),
  format('Compiling aspects into module ~w...', [Module]),
  concat_atom([Module, '_term_expansion.pl'], InitFile),
  concat_atom([Module, '_asserts.qof'], Serts),
  compile_aspect_module(Module, Date, Aspects),
  compile_term_expansion_file(InitFile, Date, Aspects),
  compile_aspect_proxy_file(Module, Date, Serts, Aspects),
  format('Use qpc -i ~w <.pl files> ~w to create an~n', [InitFile, Serts]),
  format('aspect-activated executable; recompile <.pl files> without~n', []),
  format('the ''-i ~w'' directive to deactivate aspects.~n', [InitFile]).

% -------------------------------------------------------
% INTERNAL IMPLEMENTATION PREDICATES

:- dynamic aspect_def/3.
aspect_def(nil, nil, nil).
%    Stores information on aspects for retrieval by program/1

:- dynamic aspect_decl/1.
aspect_decl(nil).
%    Stores declarations to preface the aspect proxy file.

harvest_aspects([Aspect|Rest]) :-
%    Collects the aspects accumulated in the prolog database
  findall(aspect(Type, Call, Code), 
	  (aspect_def(Type, Call, Code),
	   list_utils:takeout(Type, [before, after, around], _)),
	  [Aspect|Rest]).

compile_aspect_module(Module, Date, Aspects) :-
  concat_atom([Module, '.pl'], ModuleFile),
  tell(ModuleFile),
    write_file_header(module, "", Module, Date),
    format(':- module(~w, [~n', [Module]),
    remove_duplicate_headers(Aspects, [Aspect|Rest], []),
    write_pred_decl(Aspect),
    write_pred_decls(Rest),
    format('~n]).~2n', []),
    write_declarations,
    write_pred_defs(Aspects, Arounds, []),
    write_prove_goals(Aspects, Arounds),
  told.

compile_term_expansion_file(InitFile, Date, Aspects) :-
  format('done~nCreating initialization file ~w...', [InitFile]),
  tell(InitFile),
    write_file_header(file, "  ", InitFile, Date),
    format(':- multifile term_expansion/2.~2n', []),
    write_term_expansion_pairs(Aspects),
  told.

compile_aspect_proxy_file(Module, Date, Serts, Aspects) :-
  concat_atom([Module, '_asserts.pl'], AssertsFile),
  format('done.~nCreating and compiling ~w...', [AssertsFile]),
  tell(AssertsFile),
    write_file_header(file, "  ", AssertsFile, Date),
    write_proxies(Aspects, Module),
  told,
  format('created...', []),
  system([qpc, ' -c ', AssertsFile]),
  format('compiled...', []),
  format('done.~n~w compiled to ~w.~n', [AssertsFile, Serts]).

write_file_header(Type, Spacer, Name, Date) :-
%    Writes the standard header information for file identification.
  format('% ~w:~s    ~w~n', [Type, Spacer, Name]),
  format('% date:      ~w~2n', [Date]),
  format('% ~w generated by aspects:program/1~2n', [Type]).

% write_pred_decls/1 creates a predicate declaration for exported aspects.

write_pred_decls([Aspect|Aspects]) :-
  format(',~n', []),
  write_pred_decl(Aspect),
  write_pred_decls(Aspects).
write_pred_decls([]).

write_pred_decl(aspect(Type, Module:Callable, _)) :-
%    Writes out the aspect predicate declaration of Callable
  predicate_info(Callable, Fn, N),
  format('     ~w_~w_~w_aspect/~d', [Type, Module, Fn, N]).

predicate_info(Callable, Name, Arity) :-
%    Extracts the name and arity (the generalisation) of Callable
  Callable =.. [Name|Args],
  length(Args, Arity).

write_declarations :-
  findall(Decl, aspect_decl(Decl), Decls),
  takeout(nil, Decls, Declarations),
  map(phi(X, X, format(':- ~q.~n', [X])), Declarations, _),
  nl.

write_pred_defs([aspect(Type, Module:Callable, Block)|Aspects]) -->
  { Callable =.. [Fn|Args],
    concat_atom(['_', Module, '_', Fn], InternalName),
    concat_atom([Type, InternalName, '_aspect'], Name),
    Head =.. [Name|Args],
    format('~q :-~n', [Head]) },
  print_body(Type, Block, Module:Fn, Args),
  write_pred_defs(Aspects).
write_pred_defs([]) --> [].

% For before and after aspects, and for around aspects that call the
% primary predicate decorated with either a before or an after aspect
% (or both), we need to create a predicate that captures the before,
% primary, and after execution; write_prove_goals/2 does this.

write_prove_goals([aspect(Type, Module:Callable, _)|Aspects], Arounds) :-
  Type <- [before, after],
  !,
  write_prove_goal_head(Module:Callable, InternalName, Fn, Args),
  prove_goal_body(Type, Module:Fn, InternalName, Args, Block),
  print_body(Type, Block, Module:Fn, Args, _, []),
  % Let's not repeat this process for other aspects of the same type/sig:
  filter(Aspects, aspect(Type, Module:Callable, _), _, Rest),
  write_prove_goals(Rest, Arounds).

write_prove_goals([aspect(around, Module:Callable, _)|Aspects], Arounds) :-
%    around aspects do on occasion call the primary predicate, in that
%    case where the primary predicate is also decorated by a before or
%    an after aspect, we need to create a proxy that calls these
%    aspects and the primary predicate appropriately.
  predicate_info(Callable, Fn, N),
  ((Module:Fn)/N) <- Arounds,
  filter(Aspects, aspect(_, Module:Callable, _), OtherAspects, Rest),
  filter(OtherAspects, aspect(around, Module:Callable, _),
	 _, [Before|Others]),
  !,
  write_prove_goal_head(Module:Callable, InternalName, Fn, Args),
  write_before_goal_target(InternalName, Args, [Before|Others], After),
  write_prove_goal_target(Module:Fn, Args),
  write_after_goal_target(After, InternalName, Args),
  format('.~2n', []),
  write_prove_goals(Rest, Arounds).
write_prove_goals([aspect(around, _, _)|Aspects], Arounds) :-
%    We failed the previous clause because this around aspect either
%    does not call the primary predicate at all or it calls it
%    directly (because there are neither a before nor an after aspect
%    for the predicate.  Because of this, we do not need a
%    prove_<module>_<functor>_goal proxy predicate.
  write_prove_goals(Aspects, Arounds).
write_prove_goals([], _).

write_prove_goal_head(Module:Callable, InternalName, Fn, Args) :-
  Callable =.. [Fn|Args],
  concat_atom([Module, '_', Fn, '_'], InternalName),
  concat_atom([prove_, InternalName, goal], HeadName),
  Head =.. [HeadName|Args],
  format('~q :-~n', [Head]).

prove_goal_body(Type, Name, InternalName, Args, Block) :-
  prove_goal_target(Name, Args, Goal),
  aspect_target_body(Type, InternalName, Args, Goal, Block).

prove_goal_target(Module:Name, Args, Module:Goal) :-
  concat_atom([Name, '_aspect_target'], GoalName),
  Goal =.. [GoalName|Args].

% write_*_goal_target predicates write out the calls to the appropriate
% call-outs (before, actual, and after goals) for the around aspect

write_before_goal_target(InternalName, Args) -->
  takeout(aspect(before, _, _)),
  !,
  { concat_atom([before_, InternalName, aspect], Prelude),
    Prologue =.. [Prelude|Args],
    format('  ~q,~n', [Prologue]) }.
write_before_goal_target(_, _) --> [].

write_prove_goal_target(Name, Args) :-
  prove_goal_target(Name, Args, Goal),
  format('  ~q', [Goal]).

write_after_goal_target([aspect(after, _, _)], InternalName, Args) :-
  concat_atom([after_, InternalName, aspect], Postlude),
  Epilogue =.. [Postlude|Args],
  format(',~n  ~q', [Epilogue]).
write_after_goal_target([], _, _).

% print_body/6 destructures the Callable argument of the aspect in
% the operator precedence of Prolog (first ;/2 (or |/2) terms, next
% ->/2 terms, then ,/2 terms, finally singleton goals.  We are
% particularly interested in finding the prove_goal/[0,N] "call" 
% and replacing that "call" with the redirection to the goal under
% aspect advisement.

print_body(Type, Body, Pred, Args) -->
  print_body_aux(Type, Body, Pred, Args),
  { format('.~2n', []) }.

% First up, destructuring [;,|]/2 terms:

print_body_aux(around, Semis, Pred, Args) -->
  { Semis = (ProveGoal; Goals); Semis = (ProveGoal | Goals) },
  prove_goal_arity_n(ProveGoal, Args, Pred, ArgsUsed),
  !,
  { write('('),
    print_prove_goal_call(Pred, ArgsUsed),
    format(';~n', []) },
  print_body_aux(around, Goals, Pred, Args),
  { write(')') }.
print_body_aux(Type, (Goal; Goals), Pred, Args) -->
  { Semis = (Goal; Goals); Semis = (Goal | Goals) },
  !,
  { write('  (') },
  print_body_aux(Type, Goal, Pred, Args),
  { format(';~n', []) },
  print_body_aux(Type, Goals, Pred, Args),
  { write(')') }.

% Next, destructuring (->)/2 terms:

print_body_aux(around, (ProveGoal -> Goals), Pred, Args) -->
  prove_goal_arity_n(ProveGoal, Args, Pred, ArgsUsed),
  !,
  { print_prove_goal_call(Pred, ArgsUsed),
    format(' ->~n', []) },
  print_body_aux(around, Goals, Pred, Args).
print_body_aux(Type, (Goal -> Goals), Pred, Args) -->
  !,
  print_body_aux(Type, Goal, Pred, Args),
  { format(' ->~n', []) },
  print_body_aux(Type, Goals, Pred, Args).

% Then, destructuring (,)/2 terms:

print_body_aux(around, (ProveGoal, Goals), Pred, Args) -->
  prove_goal_arity_n(ProveGoal, Args, Pred, ArgsUsed),
  !,
  { print_prove_goal_call(Pred, ArgsUsed),
    format(',~n', []) },
  print_body_aux(around, Goals, Pred, Args).
print_body_aux(Type, (Goal, Goals), Pred, Args) -->
  !,
  print_body_aux(Type, Goal, Pred, Args),
  { format(',~n', []) },
  print_body_aux(Type, Goals, Pred, Args).

% Finally, singleton terms:

print_body_aux(around, ProveGoal, Pred, Args) -->
  prove_goal_arity_n(ProveGoal, Args, Pred, ArgsUsed),
  !,
  { print_prove_goal_call(Pred, ArgsUsed) }.
print_body_aux(_, Goal, _, _) --> { format('  ~q', [Goal]) }.

prove_goal_arity_n(ProveGoal, Args, Pred, ArgsUsed) -->
%    A little test to see if this term is a prove_goal/[0,N] "term".
%    If it is, assert that into the DCG because we are interested later
%    in around goals that make this "call".
  { ProveGoal =.. [prove_goal|OtherArgs],
    length(OtherArgs, N),
    length(Args, Len),
    same_arg_length(N, Len, Pred, Args, OtherArgs, ArgsUsed) },
  [Pred/Len].

% same_arg_length/6 demands that prove_goal is either nullary or
% arity N (where N is the arity of the advised goal).  If this
% prove_goal is an entirely different arity, quit with an exception.

same_arg_length(X, X, _, _) --> !.
same_arg_length(0, _, _, Args, _, Args) :- !.
same_arg_length(N, Len, Pred, _, _, _) :-
   (N < Len; N > Len),
   raise_exception(incorrect_arity(prove_goal/N, for(around(Pred/Len)))).

print_prove_goal_call(Module:Fn, Args) :-
  aspect_def(Type, Module:Callable, _),
  Type <- [before, after],
  length(Args, N),
  predicate_info(Callable, Fn, N),
  !,
  concat_atom([prove_, Module, '_', Fn, '_goal'], Name),
  Goal =.. [Name|Args],
  format('  ~q', [Goal]).
print_prove_goal_call(Module:Fn, Args) :-
  write_prove_goal_target(Module:Fn, Args).

% aspect_target_body/5 provides the body of the target of the aspects
% modifying the goal.

aspect_target_body(before, InternalName, Args, Goal, (Prologue, Goal)) :-
%    The before aspect executes Prologue then executes Goal
  concat_atom([before_, InternalName, aspect], Prelude),
  Prologue =.. [Prelude|Args].

aspect_target_body(after, InternalName, Args, Goal, (Goal, Epilogue)) :-
%    The after aspect executes Epilogue after Goal, but doesn't
%    alter any of the unifications of args of Goal.
  concat_atom([after_, InternalName, aspect], Postlude),
  Epilogue =.. [Postlude|Args].

% write_term_expansion_pairs/1 writes out the various term_expansion/2
% declarations for the aspect-advised predicates.

write_term_expansion_pairs([aspect(_, Module:Callable, _)|Aspects]) :-
  filter(Aspects, aspect(_, Module:Callable, _), _Covered, Rest),
  copy_term(Callable, Goal),
  Goal =.. [Fn|Args],
  concat_atom([Fn, '_aspect_target'], TargetName),
  Target =.. [TargetName|Args],
  write_term_expansion_pair(Goal, Target),
  write_dcg_expansion(Args, Fn, TargetName),
  write_term_expansion_pairs(Rest).
write_term_expansion_pairs([]).

write_term_expansion_pair(Goal, Target) :-
  format('term_expansion(~q,~n~15c~q).~n', [Goal, 32, Target]),
  format('term_expansion((~q :- Body),~n~15c(~q :- Body)).~2n',
	 [Goal, 32, Target]).

% write_dcg_expansion/3 checks that there are at least two arguments
% for this goal, and, if so, provides a term_expansion rule that
% handles the possibility that it may be represented as a DCG.

write_dcg_expansion([_, _|Args], GoalName, TargetName) :-
%    note that since are the arguments have been copied to anonymous
%    variables, we don't particularly care that we're sending the
%    "wrong" arguments to the DCG query.
  !,
  Goal =.. [GoalName|Args],
  Target =.. [TargetName|Args],
  format('term_expansion((~q --> RHS), Clause) :-~n', [Goal]),
  format('  expand_term((~q --> RHS), Clause).~2n', [Target]).
write_dcg_expansion(_, _, _).

% write_proxies/2 replaces the original predicates with proxies so that
% the aspects may do their work when the program calls the goal.

write_proxies([aspect(Type, Module:Callable, _)|Aspects], AspectModule) :-
  copy_term(Callable, Goal),
  filter([aspect(Type, _, _)|Aspects], aspect(_, Module:Goal, _),
	 Covered, Rest),
  Goal =.. [Fn|Args],
  (aspect(around, _, _) <- Covered ->
     concat_atom([around_, Module, '_', Fn, '_aspect'], Name)
   ;
     concat_atom([prove_, Module, '_', Fn, '_goal'], Name)),
  Aspect =.. [Name|Args],
  format('~w:~q :-~n  ~w:~q.~2n', [Module, Goal, AspectModule, Aspect]),
  write_proxies(Rest, AspectModule).
write_proxies([], _).

% We only wish to write one header for each aspect-advised predicate, so
% remove_duplicate_headers/3 ensures we do this by eliminating multiple
% aspects on the same predicate (this is only for headers, mind you).

remove_duplicate_headers([Aspect|Aspects]) -->
  { Aspect = aspect(Type, Sig, _),
    filter(Aspects, aspect(Type, Sig, _), _, Rest) },
  [Aspect],
  remove_duplicate_headers(Rest).
remove_duplicate_headers([]) --> [].
