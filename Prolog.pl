:- use_module(library(clpfd)).
:- use_module(library(clpb)).

% Question 1

% Predicate to check if the list contains the element.
% Checks to see if element X is at the head of the list; if not, recursively tries to find X in the tail of the list.
elem(X, [X | _]).
elem(X, [_ | Tail]) :- elem(X, Tail).

% Test Cases for elem(X,Xs)
:- elem(1,[5,1,2,3]).
:- elem(0,[10,132,4209,0101,0]).
:- \+elem(1,[2,3]).

% Predicate to check if Ys is equal to Xs with one instance of X removed.
% If X is an element of the first list Xs, the predicate removes one instance of X and checks if the resulting list is equal to Ys.
pick(X, [X | Xs], Xs).
pick(X, [Y | Xs], [Y | Ys]) :-
    pick(X, Xs, Ys).

% Test Cases for pick(X, Xs, Ys)

% Example tests from the question:
% :- pick(1, [1,2,3], [2,3]).
% :- \+ pick(1, [2,3], [2,3]).
% :- pick(1, [2,1,2,1,3], [2,2,1,3]).
% :- pick(1,[2,1,2,1,3], [2,1,2,3]).
% :- \+ pick(1,[2,1,2,1,2], [2,2,3]).

% Additional test cases:
:- pick(1,[1],[]).
:- pick(-20,[-20,1],[1]).
:- pick(5,[5,5,5,5,5],[5,5,5,5]).
:- pick(9,[1,2,3,4,5,6,7,8,9],[1,2,3,4,5,6,7,8]).
:- pick(4,[1,1,2,2,3,3,4,4,5,5], [1,1,2,2,3,3,4,5,5]).

% Predicate to check if Xs is a permutation of Ys.
permute([], []).
permute(Xs, [Y | Ys]) :-
    select(Y, Xs, Rest),
    permute(Rest, Ys).

% Test Cases for permute(Xs, Ys)
:- permute([100],[100]).
:- permute([1,2,3,4,5], [5,1,4,2,3]).
:- permute([],[]).
:- permute([1,1,1,1,1], [1,1,1,1,1]).
:- \+ permute([1,2,3,4,4], [1,2,3,4]).

% Predicate to check if given list is sorted in ascending order.
% Iterates through the list, checking, for each pair X , Y --> X <= Y.
sorted([]).
sorted([_]).
sorted([X, Y | Rest]) :-
    X =< Y,
    sorted([Y | Rest]).

% Test Cases for sorted(Xs)
:- sorted([-5,-3,-2,-1,0]).
:- sorted([]).
:- sorted([-2,-1,0,1,2]).
:- sorted([0.2,0.4,0.6,0.8,1]).
:- \+ sorted([100,20,10]).

% Predicate to check if list Ys is a sorted permutation of list Xs.
% Given 2 lists Xs and Ys -- naive_sort(Xs,Ys) first checks if the lists are permutations, then checks if Ys is sorted.
naive_sort([],[]).
naive_sort([_], [_]).
naive_sort(Xs, Ys) :- permute(Xs, Ys), sorted(Ys).

% Test Cases for naive_sort(Xs)
:- naive_sort([54,32,88,90,11], [11,32,54,88,90]).
:- naive_sort([0,-1,2,-2,1],[-2,-1,0,1,2]).
:- naive_sort([50],[50]).
:- naive_sort([0.22, 0.09, 1.25, 2.59], [0.09, 0.22, 1.25, 2.59]).
:- naive_sort([1,2],[1,2]).

% Question 2

% Clue 2
clue(Alice, Bob) :- sat(Alice =:= ~Alice * ~Bob).

% riddle 1
% Alice says “Bob is a goblin”. Bob says “Alice and I are gnomes”.
% Prolog predicate to determine if Alice and Bob are goblins or gnomes!
riddle_1(Alice, Bob) :- 
    sat(Alice =:= ~Bob),
    sat(Bob =:= Alice * Bob).

% Testing the solution for riddle 1
:- riddle_1(Alice, Bob).

% riddle 2
% Alice says “Dave is a gnome”.
% Bob says “Carol is a goblin and Alice is a gnome”.
% Carol says “I am a goblin or I am a gnome”
% Dave says “Exactly 2 of these statements are true: Alice is a gnome, Alice is a goblin, Bob and Carol are goblins”
% Prolog predicate to determine if Alice, Bob, Carol and Dave are goblins or gnomes!
riddle_2(Alice, Bob, Carol, Dave) :-
    sat(Alice =:= Dave),
    sat(Bob =:= ~Carol * Alice),
    sat(Carol =:= Carol + ~Carol),
    sat(Dave =:= Alice # ~Alice # (~Bob * ~Carol)).

% Testing the solution for riddle 2
:- riddle_2(Alice, Bob, Carol, Dave).

% riddle 3
is_creature(X) :- var(X).

is_statement(gnome(X)):- is_creature(X).
is_statement(goblin(X)):- is_creature(X).
is_statement(and(X,Y)):-
    is_statement(X),
    is_statement(Y).
is_statement(or(X,Y)):-
    is_statement(X),
    is_statement(Y).

extractedstatement(gnome(A), A).
extractedstatement(goblin(B), ~B).
extractedstatement(and(A,B), (X * Y)) :- extractedstatement(A , X), extractedstatement(B , Y).
extractedstatement(or(A,B), (X + Y)) :- extractedstatement(A , X), extractedstatement(B , Y).

goblins_or_gnomes( [_] , []). %this is a base case
goblins_or_gnomes([] , []). 
goblins_or_gnomes([G|Gs] , [R|Rs]):-
    extractedstatement(R, V), sat(G =:= V), goblins_or_gnomes(Gs, Rs).

:- goblins_or_gnomes([Alice, Bob, Carol] , [and(gnome(Bob), goblin(Alice)), goblin(Carol)]).
:- goblins_or_gnomes([Alice, Bob] , [or(gnome(Alice), goblin(Alice)), and(gnome(Bob), goblin(Alice))]).
:- goblins_or_gnomes([Alice, Bob, Carol, Dave] , [gnome(Dave), goblin(Dave), or(gnome(Alice), goblin(Bob)), gnome(Alice)]).
:- goblins_or_gnomes([Alice, Bob], [or(goblin(Alice), gnome(Bob)), or(goblin(Alice), goblin(Alice))]).
:- goblins_or_gnomes([Alice, Bob], [and(goblin(Bob), gnome(Bob)), and(goblin(Alice), gnome(Alice))]).

boolean(true).
boolean(false).

is_expr(int(V)) :- V in inf..sup.
is_expr(bool(B)) :- boolean(B).
is_expr(add(X,Y)) :- is_expr(X), is_expr(Y).
is_expr(mul(X,Y)) :- is_expr(X), is_expr(Y).
is_expr(neg(X)) :- is_expr(X).
is_expr(and(X,Y)) :- is_expr(X), is_expr(Y).
is_expr(xor(X,Y)) :- is_expr(X), is_expr(Y).
is_expr(if(B,X,Y)) :- is_expr(B), is_expr(X), is_expr(Y).

is_val(V) :- boolean(V); V in inf..sup.


% Define the evaluation rules
eval_expr(int(V), V).
eval_expr(bool(true), true). %:- boolean(true).
eval_expr(bool(false), false). %:- boolean(false).

% Tests for evaluation.
:- eval_expr(int(1), 1).
:- eval_expr(int(2), 2).
:- eval_expr(int(-5), -5).
:- eval_expr(int(-10), -10).
:- eval_expr(bool(true), true).
:- eval_expr(bool(false), false).

% Takes 2 integers X, Y as input and calculates their sum.
eval_expr(add(X, Y), V) :-
    eval_expr(X, XV),
    eval_expr(Y, YV),
    V #= XV + YV.

% Tests for add
:- eval_expr(add(int(1), int(0)), 1).
:- eval_expr(add(int(-15), int(5)), -10).
:- eval_expr(add(int(-20), int(-30)), -50).

% Takes 2 integers X, Y as input and calculates their product.
eval_expr(mul(X, Y), V) :-
    eval_expr(X, XV),
    eval_expr(Y, YV),
    V #= XV * YV.

% Tests for mul
:- eval_expr(mul(int(1), int(0)), 0).
:- eval_expr(mul(int(-5), int(2)), -10).
:- eval_expr(mul(int(-20), int(-30)), 600).

% Tests to build my student ID -- 400357483
:- eval_expr(add(int(400357000), int(483)), 400357483).                                                 % 400357000 + 483 = 400357483
:- eval_expr(add((mul(int(200000000), int(2))), int(357483)), 400357483).                               % (200000000 * 2) + 357483 = 400357483
:- eval_expr(add((add(int(100000000), int(300000000))), (add(int(350000), int(7483)))), 400357483).     % (100000000 + 300000000) + (350000 + 7483) = 400357483

% Takes an integer X as input and negates the number.
eval_expr(neg(X), V) :-
    eval_expr(X, XV),
    V #= -XV.

% Tests for neg
:- eval_expr(neg(int(-100)), 100).
:- eval_expr(neg(int(44400)), -44400).
:- eval_expr(neg(neg(int(20))), 20). % (-(-(20)))

% Takes 2 booleans as input and computed the logical 'and' operator.
eval_expr(and(X,Y) , V):-
    eval_expr(X, VX),
    eval_expr(Y, VY),
    ((VX, VY) -> V = true; V = false).

% Tests for and
:- eval_expr(and(bool(true),bool(false)), false).
:- eval_expr(and(bool(false),bool(false)), false).
:- eval_expr(and(bool(false),bool(true)), false).
:- eval_expr(and(bool(true),bool(true)), true).
:- eval_expr(and(and(bool(true),bool(false)), bool(true)), false). % Test for nested and

% Takes 2 booleans as input and computed the logical 'xor' operator.
eval_expr(xor(X,Y),false):-
    eval_expr(X, false),
    eval_expr(Y, false).

eval_expr(xor(X,Y),false):-
    eval_expr(X, true), 
    eval_expr(Y, true).

eval_expr(xor(_, _), true).

% Tests for xor
:- eval_expr(xor(bool(true),bool(false)), true).
:- eval_expr(xor(bool(false),bool(false)), false).
:- eval_expr(xor(bool(false),bool(true)), true).
:- eval_expr(xor(bool(true),bool(true)), false).
:- eval_expr(xor(xor(bool(true),bool(false)), bool(true)), false). % Test for nested xor

% Takes a boolean condition, and 2 expressions as input.
% If the condition is true -- returns first expression.
% If false -- returns the second expression.
eval_expr(if(B, X, Y), V) :-
    eval_expr(B, BV),
    eval_expr(X, XV),
    eval_expr(Y, YV),
    (BV -> V = XV ; V = YV).

% Tests for if 
:- eval_expr(if(bool(true),int(4),int(9)), 4).                                                          % If true, then 4 else 9
:- eval_expr(if(bool(true), add(int(4), int(6)), add(int(-4), int(-6))), 10).                           % If true, then (6 + 4) else (-4 + -6)
:- eval_expr(if(bool(false), add(int(4), int(6)), add(int(-4), int(-6))), -10).                         % 
:- eval_expr(if(and(bool(false),bool(false)), mul(int(1), int(34)), add(int(4), int(-6))), -2).
:- eval_expr(if(xor(bool(false),bool(false)), mul(int(1), int(34)), add(int(4), int(-6))), 34).