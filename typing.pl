% I am Insiyah Ujjainwala
% ujjainwi@mcmaster.ca

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- use_module(library(clpb)).
:- use_module(library(clpfd)).

% Base case: Removing an element from an empty list results in an empty list
remove(_, [], []).
remove(X, [X | Tail], Result) :-
    remove(X, Tail, Result).
remove(X, [Y | Tail], [Y | Result]) :-
    X \= Y,
    remove(X, Tail, Result).

% Test cases for remove
:- remove(1,[1,1,1,1,1],[]).
:- remove(3, [1,3,2,3,4,3,5], [1,2,4,5]).
:- remove(6, [1,2,3,4,5], [1,2,3,4,5]).
:- remove(0,[],[]).

expr(true).
expr(false).
expr(var(X)) :- string(X).
expr(lam(V, T, E)) :- var(V), type(T), expr(E).
expr(app(X, Y)) :- expr(X), expr(Y).
expr(pair(X, Y)) :- expr(X), expr(Y).
expr(fst(X, Y)) :- expr(X), expr(Y).
expr(snd(X, Y)) :- expr(X), expr(Y).
expr(and(X, Y)) :- expr(X), expr(Y).
expr(if(B, X, Y)) :- expr(B), expr(X), expr(Y).
expr(let(V, X, Y)) :- var(V), expr(X), expr(Y).

% Test cases for expr
:- expr(true).
:- expr(false).
:- expr(var("x")).

% context predicate to manage variable bindings
context([], _, _) :- false.
context([(Var, Value)|_], Var, Value).
context([(_, _)|Rest], Var, Value) :-
    context(Rest, Var, Value).

% Helper predicate to update a context with a new binding
update_context(Context, Var, Value, [(Var, Value)|Context]).

% Test cases for context
:- context([(var1, 10), (var2, 20)], var1, 10).
:- \+context([(var1, 10), (var2, 20)], var3, 5).
:- \+context([(var1, 10), (var2, 20)], var2, 5).
:- update_context([(var1, 10)], var2, 20, [(var2, 20),(var1, 10)]).
:- update_context([], var1, 10, [(var1, 10)]).

% Free vars from variable
free_vars(var(X), [X]).

% Free vars from lambda
free_vars(lam(V, _, E), Vars) :-
    free_vars(E, EVars),
    remove(V, EVars, Vars).

% Free vars from app
free_vars(app(E1, E2), Vars) :-
    free_vars(E1, Vars1),
    free_vars(E2, Vars2),
    union(Vars1, Vars2, Vars).

% Free vars from pair
free_vars(pair(E1, E2), Vars) :-
    free_vars(E1, Vars1),
    free_vars(E2, Vars2),
    union(Vars1, Vars2, Vars).

% Free vars from fst
free_vars(fst(E1, _), Vars) :-
    free_vars(E1, Vars).

% Free vars from snd
free_vars(snd(_, E2), Vars) :-
    free_vars(E2, Vars).

% Free vars from and
free_vars(and(E1, E2), Vars) :-
    free_vars(E1, Vars1),
    free_vars(E2, Vars2),
    union(Vars1, Vars2, Vars).

% Free vars from if
free_vars(if(E1, E2, E3), Vars) :-
    free_vars(E1, Vars1),
    free_vars(E2, Vars2),
    free_vars(E3, Vars3),
    union(Vars1, Vars2, TempVars),
    union(TempVars, Vars3, Vars).

% Free vars from let
free_vars(let(V, E1, E2), Vars) :-
    free_vars(E1, Vars1),
    free_vars(E2, Vars2),
    remove(V, Vars2, Vars2Removed),
    union(Vars1, Vars2Removed, Vars).

free_vars(true, []).
free_vars(false, []).

% Test cases for free_vars
% NOTE: I am not sure if this function has been implemented correctly.
:- free_vars(var('x'), [x]).
:- free_vars(lam('x' , type, app(var('x'), var('y'))), [y]).
:- free_vars(app(var('x'), var('y')), [x,y]).
:- free_vars(lam(x, type, var(x)), []).
:- free_vars(let('x', var('y'), var('x')), [y]).
:- free_vars(lam('x', type, lam('y', type, app(var('x'), var('y')))), []).
:- free_vars(if(lam('x', type, var('x')), app(lam('y', type, var('y')), var('z')), var('z')), [z]).

% Define the freshen predicate in Prolog
freshen(Name, Avoid, FreshName) :-
        (member(Name, Avoid) ->
        string_concat(Name, "*", NewName), 
        freshen(NewName, Avoid, FreshName);
        FreshName = Name
    ).

% Test cases for freshen
:- freshen('var', ['var1', 'var2', 'var3'], var).
:- freshen('var', ['var', 'var2', 'var3'], "var*").
:- freshen('var*', ['var*', 'var1'], "var**").
:- \+freshen('var', ['var', 'var*', 'var**'], "var***"). 
% The freshen predicate should concatenate *'s until a new name is found but I'm not sure why it's not producing the correct output

% Rename a term to not use the names in the provided list.
rename(Expr, Avoid, Renamed) :-
    go_rename(Expr, [], Avoid, Renamed).

go_rename(var(X), _, Avoid, Renamed) :-
    ( member(X, Avoid) ->
        freshen(X, Avoid, X_New),
        Renamed = var(X_New)
    ; Renamed = var(X)
    ).

go_rename(lam(V, T, E), RN, Avoid, lam(V_New, T, E_New)) :-
    freshen(V, Avoid, V_New),
    go_rename(E, [(V, V_New) | RN], Avoid, E_New).

    
go_rename(app(E1, E2), RN, Avoid, app(E_New1, E_New2)) :-
    go_rename(E1, RN, Avoid, E_New1),
    go_rename(E2, RN, Avoid, E_New2).

go_rename(pair(E1, E2), RN, Avoid, pair(E_New1, E_New2)) :-
    go_rename(E1, RN, Avoid, E_New1),
    go_rename(E2, RN, Avoid, E_New2).

go_rename(fst(E1, E2), RN, Avoid, fst(E_New1, E_New2)) :-
    go_rename(E1, RN, Avoid, E_New1),
    go_rename(E2, RN, Avoid, E_New2).

go_rename(snd(E1, E2), RN, Avoid, snd(E_New1, E_New2)) :-
    go_rename(E1, RN, Avoid, E_New1),
    go_rename(E2, RN, Avoid, E_New2).

go_rename(and(E1, E2), RN, Avoid, and(E_New1, E_New2)) :-
    go_rename(E1, RN, Avoid, E_New1),
    go_rename(E2, RN, Avoid, E_New2).

go_rename(if(E1, E2, E3), RN, Avoid, if(E_New1, E_New2, E_New3)) :-
    go_rename(E1, RN, Avoid, E_New1),
    go_rename(E2, RN, Avoid, E_New2),
    go_rename(E3, RN, Avoid, E_New3).

go_rename(let(V, E1, E2), RN, Avoid, let(V_New, E_New1, E_New2)) :-
    freshen(V, Avoid, V_New),
    go_rename(E1, RN, Avoid, E_New1),
    go_rename(E2, [(V, V_New) | RN], Avoid, E_New2).

go_rename(true, _, _, true).
go_rename(false, _, _, false).

% Test cases for rename 
:- rename(var('x'), ['y', 'z'], var('x')).
:- rename(var('x'), ['x', 'y'], var("x*")).
:- rename(app(var('x'), var('y')), ['y'], app(var('x'), var("y*"))).
:- rename(let('x', var('y'), app(var('x'), var('z'))), ['x'], let("x*", var(y), app(var("x*"), var(z)))).
:- rename(lam('x', type, lam('y', type, app(var('x'), var('y')))), ['x', 'y'], lam("x*", type, lam("y*", type, app(var("x*"), var("y*"))))).


% Define the subst predicate for variable substitution
subst(X, E1, E2, Result) :-
    subst_renamed(X, E1, E2, Result).

subst_renamed(X, E1, var(Y), Result) :-
    (X = Y -> Result = E1 ; Result = var(Y)).

subst_renamed(X, E1, lam(V, T, E), lam(V, T, E_New)) :-
    (X \= V -> subst_renamed(X, E1, E, E_New) ; E_New = lam(V, T, E)).

subst_renamed(X, E1, app(E2, E3), app(E_New2, E_New3)) :-
    subst_renamed(X, E1, E2, E_New2),
    subst_renamed(X, E1, E3, E_New3).

subst_renamed(X, E1, pair(E2, E3), pair(E_New2, E_New3)) :-
    subst_renamed(X, E1, E2, E_New2),
    subst_renamed(X, E1, E3, E_New3).

subst_renamed(X, E1, fst(E), fst(E_New)) :-
    subst_renamed(X, E1, E, E_New).

subst_renamed(X, E1, snd(E), snd(E_New)) :-
    subst_renamed(X, E1, E, E_New).

subst_renamed(X, E1, and(E2, E3), and(E_New2, E_New3)) :-
    subst_renamed(X, E1, E2, E_New2),
    subst_renamed(X, E1, E3, E_New3).

subst_renamed(X, E1, if(E2, E3, E4), if(E_New2, E_New3, E_New4)) :-
    subst_renamed(X, E1, E2, E_New2),
    subst_renamed(X, E1, E3, E_New3),
    subst_renamed(X, E1, E4, E_New4).

subst_renamed(X, E1, let(V, E2, E3), let(V, E2New, E3New)) :-
    subst_renamed(X, E1, E2, E2New),
    (X = V -> E3New = E3 ; subst_renamed(X, E1, E3, E3New)).

subst_renamed(_, _, true, true).
subst_renamed(_, _, false, false).

% Test cases for subst
:- subst('x', true, var('x'), true).
:- subst('x', true, app(var('x'), var('y')), app(true,var(y))).
:- subst('x', true, if(app(var('x'), var('z')), var('y'),var('x')),if(app(true, var(z)), var(y), true)).
:- subst('y', false, let(var('x'), var('y'), and(var('x'), var('y'))), let(var(x), false, and(var(x), false))).
:- subst('x', true, pair(var('x'), var('y')), pair(true, var(y))).
:- subst('x', true, lam('y', type, lam('x', type, var('x'))), lam(y, type, lam(x, type, lam(x, type, var(x))))).
:- subst('x', true, app(app(var('x'), var('y')), var('x')), app(app(true, var(y)), true)).
:- subst('x', true, fst(pair(var('x'), var('y'))), fst(pair(true, var(y)))).
:- subst('y', false, snd(pair(var('x'), var('y'))), snd(pair(var(x), false))).
:- subst(var('y'), false, fst(pair(var('x'), var('y'))), fst(pair(var(x), var(y)))).

% stepCBV predicate
% stepCBV for app
stepCBV(app(Fn, Arg), Result, _) :-
    (stepCBV(Fn, Value_Fn, _) -> Result = app(Value_Fn, Arg)
    ;(value(Arg) ->
        (Fn = lam(X , _ , Body) -> subst(X, Arg, Body, Result))
      ;stepCBV(Arg, Value_Arg, _) -> Result = app(Fn, Value_Arg))).

% stepCBV for boolean
stepCBV(true, true, _).
stepCBV(false, false, _).

% stepCBV for and
stepCBV(and(true, E2), E2, _).
stepCBV(and(false, _), false, _).
stepCBV(and(E1, E2), Result, _) :-
    ( stepCBV(E1, Red_E1, _) -> Result = and(Red_E1, E2)
    ; fail
    ).

% stepCBV for pair
stepCBV(pair(E1, E2), pair(E1, E2), _). % No reduction possible for pair itself

% stepCBV for fst and snd
stepCBV(fst(pair(E1, _)), E1, _).
stepCBV(snd(pair(_, E2)), E2, _).

stepCBV(fst(E), Result, _) :-
    stepCBV(E, EPrime, _),
    Result = fst(EPrime).

stepCBV(snd(E), Result, _) :-
    stepCBV(E, EPrime, _),
    Result = snd(EPrime).

stepCBV(if(true, E2, _), E2, _).
stepCBV(if(false, _, E3), E3, _).
stepCBV(if(E1, E2, E3), Result, _) :-
    ( stepCBV(E1, E1Prime, _) -> Result = if(E1Prime, E2, E3)
    ; fail
    ).

stepCBV(let(V, E1, E2), Result, _) :-
    stepCBV(E1, E1Evaluated, _),
    subst(V, E1Evaluated, E2, Result).


% Test cases for stepCBV
% NOTE: I was unsure how to translate those expressions that evaluate to 'Nothing' in Haskell.
:- stepCBV(and(true, false), false, _).
:- stepCBV(fst(pair(true, false)), true, _).
:- stepCBV(snd(pair(true, false)), false, _).
:- stepCBV(if(true, false, true), false, _).
:- stepCBV(if(false, false, true), true, _).
:- stepCBV(let(var('X'), true, and(true, false)), and(true, false), _).
:- stepCBV(and(true, var('Y')), var('Y'), _).
:- stepCBV(and(and(false, true), var('Y')), and(false, var('Y')), _).
:- stepCBV(let('X', true, and(var('X'), false)), and(true, false), _).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Expression Language

% Helper function to check if an expression is a value.
value(true).
value(false).
value(var(_)).
value(lam(_,_,_)).
value(pair(X,Y)) :- value(X) , value(Y).

type(bool).
type(nat).
type(string).

typed(true, bool).
typed(false, bool).
typed(var(_), string).
typed(pair(E1, E2), pair(T1, T2)) :-
    typed(E1, T1),
    typed(E2, T2).
typed(fst(E1), T) :- typed(E1, pair(T, _ )).
typed(snd(E2), T) :- typed(E2, pair(_ , T)).
typed(lam(V, T, E),T) :- typed(V, string),typed(T, T), typed(E, T).
typed(if(B,X,Y), T) :-
  typed(B,bool), typed(X, T), typed(Y, T).
typed(let(V, E1, E2), T) :- 
    typed(V, string),
    typed(E1, T),              
    typed(E2, T).

% Test cases for typed
:- typed(true, bool).
:- typed(var('x'), string).
:- typed(fst(pair(var('x'), var('y'))), string).
:- typed(fst(pair(true, false)), bool).
:- typed(fst(pair(true, var('y'))), bool).
:- typed(pair(true, var('y')), pair(bool, string)).
:- typed(pair(true, false), pair(bool, bool)).
:- typed(pair(var('x'), var('y')), pair(string, string)).
:- typed(snd(pair(var('x'), var('y'))), string).
:- typed(snd(pair(var('x'), true)), bool).
:- typed(snd(pair(false, true)), bool).

% sstep predicate.
% sstep for app
sstep(app(E1, E2), Result) :-
    ( sstep(E1, E1Step) -> Result = app(E1Step, E2)
    ; ( sstep(E2, E2Step) -> Result = app(E1, E2Step)
      ; ( E1 = lam(V, _, Body) -> subst(V, E2, Body, Result)
        ; fail
      )
    )
    ).

% sstep for pair
sstep(pair(E1, E2), pair(E1Step, E2Step)) :-
    sstep(E1, E1Step),
    sstep(E2, E2Step).

% sstep for fst
sstep(fst(pair(E1, _)), E1).

% sstep for snd
sstep(snd(pair(_, E2)), E2).

% sstep for and
sstep(and(true, E2), E2).
sstep(and(false, _), false).
sstep(and(_, false), false).

% sstep for if
sstep(if(true, E2, _), E2).
sstep(if(false, _, E3), E3).
sstep(if(E1, E2, E3), if(E1Step, E2, E3)) :-
    sstep(E1, E1Step).

% sstep for let
sstep(let(V, E1, E2), Result) :-
    subst(V, E1, E2, Result).

% Test cases for sstep
:- \+sstep(true, true).
:- \+sstep(false, false).
:- \+sstep(var('x'), 'x').
:- \+sstep(lam(var('x'), string, 'x'), _).
:- sstep(and(false, true), false).
:- sstep(if(true, var('x'), var('y')), var(x)).
:- sstep(let('x', true, var('x')), true).
:- sstep(fst(pair(true, false)), true).
:- sstep(snd(pair(true, false)), false).
:- sstep(let('x', true, var('x')), true).
:- sstep(let('x', true, let('y', var('x'), var('y'))), let(y, true, var(y))).
:- sstep(let('x', true, lam('y', type, var('x'))), lam(y, type, true)).
:- sstep(if(true, let('x', true, var('x')), false), let(x, true, var(x))).



%% multi-step (unchanged from original typing.pl)
mstep(X, X) :- value(X).
mstep(X, Y) :- sstep(X, Z), mstep(Z, Y).

% Test cases for mstep
:- mstep(app(lam('x', type, var('x')), true), true).
:- mstep(let('x', let('y', true, var('y')), var('x')), true).
:- mstep(if(and(true, false), var('x'), false), false).
:- mstep(if(let('x', false, var('x')), true, and(var('y'), false)), false).
:- mstep(if(let('x', true, var('x')), false, and(true, true)), false).


% tsstep for app
tsstep(app(E1, E2), Result, e_App(T)) :-
    ( tsstep(E1, E1Step, T1) -> Result = app(E1Step, E2), T = e_AppL(T1)
    ; ( tsstep(E2, E2Step, T2) -> Result = app(E1, E2Step), T = e_AppR(T2)
      ; ( E1 = lam(V, _, Body) -> subst(V, E2, Body, Result), T = e_AppLambda
        ; fail
      )
    )
    ).
% tsstep for pair
tsstep(pair(E1, E2), pair(E1Step, E2Step), e_Pair(T1, T2)) :-
    tsstep(E1, E1Step, T1),
    tsstep(E2, E2Step, T2).

% tsstep for fst
tsstep(fst(pair(E1, _)), E1, e_Fst).

% tsstep for snd
tsstep(snd(pair(_, E2)), E2, e_Snd).

% tsstep for and
tsstep(and(true, E2), E2, e_AndTrue).
tsstep(and(false, _), false, e_AndFalse).
tsstep(and(E1, E2), and(E1Step, E2), e_And(T)) :-
    tsstep(E1, E1Step, T).

% tsstep for if
tsstep(if(true, E2, _), E2, e_IfTrue).
tsstep(if(false, _, E3), E3, e_IfFalse).
tsstep(if(E1, E2, E3), if(E1Step, E2, E3), e_If(T)) :-
    tsstep(E1, E1Step, T).

% tsstep for let
tsstep(let(V, E1, E2), Result, e_Let) :-
    subst(V, E1, E2, Result).

% Test cases for tsstep
:- tsstep(app(lam('x', string, var('x')), true), true, e_App(e_AppLambda)).
:- tsstep(fst(pair(true, false)), true, e_Fst).
:- tsstep(and(true, false), false, e_AndTrue).
:- tsstep(if(true, false, true), false, e_IfTrue).
:- tsstep(let('x', true, var('x')), true, e_Let).


% typederiv for boolean 
typederiv(true, bool, t_True, _).
typederiv(false, bool, t_False, _).

% typederiv for var 
typederiv(var(X), Type, t_Var, Context) :-
    context(Context, X, Type).

% typederiv for lambda 
typederiv(lam(V, T, E), arrow(T, EType), t_Lam(EDeriv), Context) :-
    update_context(Context, V, T, NewContext),
    typederiv(E, EType, EDeriv, NewContext).

% typederiv for app 
typederiv(app(E1, E2), T, t_App(E1Deriv, E2Deriv), Context) :-
    typederiv(E1, arrow(TArg, T), E1Deriv, Context),
    typederiv(E2, TArg, E2Deriv, Context).

% typederiv for pair 
typederiv(pair(E1, E2), pair(T1, T2), t_Pair(E1Deriv, E2Deriv), Context) :-
    typederiv(E1, T1, E1Deriv, Context),
    typederiv(E2, T2, E2Deriv, Context).

% typederiv for fst 
typederiv(fst(E), T, t_Fst(EDeriv), Context) :-
    typederiv(E, pair(T, _), EDeriv, Context).

% typederiv for snd 
typederiv(snd(E), T, t_Snd(EDeriv), Context) :-
    typederiv(E, pair(_, T), EDeriv, Context).

% typederiv for and 
typederiv(and(E1, E2), bool, t_And(E1Deriv, E2Deriv), Context) :-
    typederiv(E1, bool, E1Deriv, Context),
    typederiv(E2, bool, E2Deriv, Context).

% typederiv for if 
typederiv(if(E1, E2, E3), T, t_If(E1Deriv, E2Deriv, E3Deriv), Context) :-
    typederiv(E1, bool, E1Deriv, Context),
    typederiv(E2, T, E2Deriv, Context),
    typederiv(E3, T, E3Deriv, Context).

% typederiv for let 
typederiv(let(V, E1, E2), T, t_Let(E1Deriv, E2Deriv), Context) :-
    typederiv(E1, T1, E1Deriv, Context),
    update_context(Context, V, T1, NewContext),
    typederiv(E2, T, E2Deriv, NewContext).

% Test cases for typederiv

:- typederiv(true, bool, t_True, []).
:- typederiv(false, bool, t_False, []).
:- typederiv(pair(true, false), pair(bool, bool), t_Pair(t_True, t_False), []).
:- typederiv(fst(pair(true, false)), bool, t_Fst(t_Pair(t_True, t_False)), []).
:- typederiv(if(true, false, true), bool, t_If(t_True, t_False, t_True), []).
