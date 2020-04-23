program(P) --> commands(P).

eval_program(P,Y,Z) :- eval_command(P,Y,Z).

%--------------------------------------------------------------------------------
commands(t_command(X,Y)) --> command(X), commands(Y).
commands(t_command(X)) --> command(X).

%command(X) --> block(X).
command(t_command_assign(Y)) --> assign(Y), [;].
%command(t_command_assignBool(Y)) --> assignBool(Y), [;].

command(t_command_while(X,Y)) --> 
    [while], booleanBool(X), ['{'], commands(Y), ['}'].

command(t_command_ternary(I,X,E1,E2)) -->
    word(I),[=], booleanBool(X),[?],expr(E1),[:],expr(E2),[;].

command(t_command_if(X,Y)) --> 
    [if], booleanBool(X), ['{'], commands(Y), ['}'].
command(t_command_ifel(X,Y,Z)) --> 
    [if], booleanBool(X), ['{'], commands(Y), ['}'], command_el(Z).

command(t_command_for_range(X,Y,Z,T))--> 
    [for], word(X), [in], [range],['('],expr(Y),expr(Z),[')'],
    ['{'],commands(T),['}'].
command(t_command_for(X,Y,Z,T)) --> 
    [for], ['('],assign(X),[;], booleanBool(Y),[;],assign(Z),[')'],
    ['{'],commands(T),['}'].

command(t_print(X)) --> [print], ['('],printseq(X),[')'],[;].

command_el(t_command_el(X,Y)) --> 
    [elif], booleanBool(X), ['{'], commands(Y), ['}'].
command_el(t_command_el(X,Y,Z)) --> 
    [elif], booleanBool(X), ['{'], commands(Y), ['}'], command_el(Z). 
command_el(t_command_else(Y)) --> 
    [else], ['{'], commands(Y), ['}'].


% change declaration for,eval_command(P,[],0,S)).

eval_command(t_command_assign(X),Env,NewEnv) :- 
    eval_expr(X,Env,_Val,NewEnv).

eval_command(t_command(X,Y),Env,NewEnv) :- 
    eval_command(X,Env,NewEnv1), eval_command(Y,NewEnv1,NewEnv).

eval_command(t_command(X),Env,NewEnv) :- 
    eval_command(X,Env,NewEnv).

eval_command(t_command_ternary(t_word(I),X,E1,_E2),Env,NewEnv):-
    eval_boolean(X,Env,NewEnv1,true),eval_expr(E1,NewEnv1,Val,NewEnv2),update(I,Val,NewEnv2,NewEnv).
eval_command(t_command_ternary(t_word(I),X,_E1,E2),Env,NewEnv):-
    eval_boolean(X,Env,NewEnv1,false),eval_expr(E2,NewEnv1,Val,NewEnv2),update(I,Val,NewEnv2,NewEnv).
    
eval_command(t_command_if(X,Y),Env,NewEnv) :- 
    eval_boolean(X,Env,NewEnv1,true), eval_command(Y,NewEnv1,NewEnv). 

eval_command(t_command_if(X,_Y),Env,Env) :- 
    eval_boolean(X,Env,Env,false). 

eval_command(t_command_ifel(X,Y,_Z),Env,NewEnv) :- 
    eval_boolean(X,Env,NewEnv1,true), eval_command(Y,NewEnv1,NewEnv). 
eval_command(t_command_ifel(X,_Y,Z),Env,NewEnv) :- 
    eval_boolean(X,Env,NewEnv1,false),eval_command(Z,NewEnv1,NewEnv).

eval_command(t_command_el(X,Y,_Z),Env,NewEnv) :- 
    eval_boolean(X,Env,NewEnv1,true), eval_command(Y,NewEnv1,NewEnv). 
eval_command(t_command_el(X,_Y,Z),Env,NewEnv) :- 
    eval_boolean(X,Env,NewEnv1,false),eval_command(Z,NewEnv1,NewEnv).
eval_command(t_command_el(X,Y),Env,NewEnv) :- 
    eval_boolean(X,Env,NewEnv1,true),eval_command(Y,NewEnv1,NewEnv).

eval_command(t_command_else(X),Env,NewEnv) :- 
   	eval_command(X,Env,NewEnv).

eval_command(t_command_while(X,Y),Env,NewEnv) :-  
    eval_boolean(X,Env,NewEnv1,true),eval_command(Y,NewEnv1,NewEnv2),
    eval_command(t_command_while(X,Y),NewEnv2,NewEnv).
eval_command(t_command_while(X,_Y),Env,NewEnv) :-  
    eval_boolean(X,Env,NewEnv,false).

eval_command(t_command_for_range(t_word(X),Y,Z,T),Env,NewEnv) :- 
    lookup(X,Env,Val1),eval_expr(Y,Env,Val2,Env1), eval_expr(Z,Env1,Val3,Env2), 
    (between(Val2, Val3, Val1)-> eval_command(T,Env2,Env3), 
    lookup(X,Env3,Val4),Val5 is Val4 + 1, update(X,Val5,Env3,Env4),
    eval_command(t_command_for_range(t_word(X),Y,Z,T),Env4,NewEnv);
    NewEnv = Env).

eval_command(t_command_for_range(t_word(X),Y,Z,T),Env,NewEnv) :-
    \+lookup(X,Env,_Val1),eval_expr(Y,Env,Val2,Env1),update(X,Val2,Env1,Env2), 
    eval_expr(Z,Env2,_Val3,Env3), 
    eval_command(T,Env3,Env4), 
    eval_command(t_command_for_range(t_word(X),Y,Z,T),Env4,NewEnv).

eval_command(t_command_for(X,Y,Z,T), Env, NewEnv) :- 
    eval_expr(X,Env,_Val1,NewEnv1),
    eval_boolean(Y,NewEnv1,NewEnv2,true),
    eval_command(T,NewEnv2,NewEnv3),
    eval_expr(Z,NewEnv3,_Val2,NewEnv4),
    eval_command(t_command_for(Y,Z,T),NewEnv4,NewEnv).

eval_command(t_command_for(X,Y,_Z,_T), Env, NewEnv) :- 
    eval_expr(X,Env,_Val1,NewEnv1),
    eval_boolean(Y,NewEnv1,NewEnv,false).

eval_command(t_command_for(Y,Z,T), Env, NewEnv) :- 
    eval_boolean(Y,Env,NewEnv2,true),
    eval_command(T,NewEnv2,NewEnv3),
    eval_expr(Z,NewEnv3,_Val,NewEnv4),
    eval_command(t_command_for(Y,Z,T),NewEnv4,NewEnv).

eval_command(t_command_for(X,Y,_Z,_T), Env, NewEnv) :- 
    eval_command(X,Env,NewEnv1), eval_boolean(Y,NewEnv1,NewEnv,false).  
eval_command(t_command_for(Y,_Z,_T), Env, NewEnv) :- 
    eval_boolean(Y,Env,NewEnv,false).

eval_command(t_print(X),Env,NewEnv) :- eval_printseq(X, Env,NewEnv,Val),writeln(Val).
%eval_command(t_block(X,Y),Env,NewEnv):- eval_block(t_block(X,Y),Env,NewEnv).
%--------------------------------------------------------------------------------
:- table boolean/3, booleanBool/3.

boolean(t_b_true()) --> [true].
boolean(t_b_false()) --> [false].
boolean(t_b_not(X)) --> [not], boolean(X).
boolean(t_b_equals(X,Y)) --> expr(X), [==], expr(Y).
boolean(t_b_equalsBool(true, true)) --> [true], [==], [true].
boolean(t_b_equalsBool(false, false)) --> [false], [==], [false].
boolean(t_b_not_equals(X,Y)) --> expr(X), [!], [=], expr(Y).
boolean(t_b_and(X,Y)) --> boolean(X),[and],boolean(Y).
boolean(t_b_or(X,Y)) --> boolean(X),[or],boolean(Y).
boolean(t_b_l(X,Y)) --> expr(X), [<], expr(Y).
boolean(t_b_g(X,Y)) --> expr(X), [>], expr(Y).
boolean(t_b_lte(X,Y)) --> expr(X), [<=], expr(Y).
boolean(t_b_gte(X,Y)) --> expr(X), [>=], expr(Y).

booleanBool(X) --> boolean(X).
booleanBool(t_b_num(X)) --> number(X).
booleanBool(t_b_word(X)) --> word(X).

booleanBool(t_b_boolAnd(X, t_b_num(Y))) --> boolean(X), [and], number(Y).
booleanBool(t_b_boolAnd(t_b_num(X),Y)) --> number(X), [and], boolean(Y).
booleanBool(t_b_boolOr(X, t_b_num(Y))) --> boolean(X), [or], number(Y).
booleanBool(t_b_boolOr(t_b_num(X),Y)) --> number(X), [or], boolean(Y).

booleanBool(t_b_boolAnd(X, t_b_word(Y))) --> boolean(X), [and], word(Y).
booleanBool(t_b_boolAnd(t_b_word(X),Y)) --> word(X), [and], boolean(Y).
booleanBool(t_b_boolOr(X, t_b_word(Y))) --> boolean(X), [or], word(Y).
booleanBool(t_b_boolOr(t_b_word(X),Y)) --> word(X), [or], boolean(Y).

booleanBool(t_b_boolAnd(t_b_num(X),t_b_word(Y))) --> number(X), [and], word(Y).
booleanBool(t_b_boolAnd(t_b_word(X), t_b_num(Y))) --> word(X), [and], number(Y).
booleanBool(t_b_boolOr(t_b_num(X),t_b_word(Y))) --> number(X), [or], word(Y).
booleanBool(t_b_boolOr(t_b_word(X), t_b_num(Y))) --> word(X), [or], number(Y).

booleanBool(t_b_boolAnd(t_b_num(X), t_b_num(Y))) --> number(X), [and], number(Y).
booleanBool(t_b_boolAnd(t_b_word(X),t_b_word(Y))) --> word(X), [and], word(Y).
booleanBool(t_b_boolOr(t_b_num(X),t_b_num(Y))) --> number(X), [or], number(Y).
booleanBool(t_b_boolOr(t_b_word(X),t_b_word(Y))) --> word(X), [or], word(Y).

eval_boolean(t_b_num(X), Env,NewEnv, Condition) :-
    eval_expr(X,Env,Val1,NewEnv), equal(Val1, 0, Val2), not(Val2,Condition).

eval_boolean(t_b_word(X),Env,NewEnv,Condition) :-
	eval_expr(X,Env,Condition,NewEnv).

eval_boolean(t_b_boolAnd(X,Y),Env,NewEnv,Condition) :-
	eval_boolean(X,Env,Env1,Val1),eval_boolean(Y,Env1,NewEnv,Val2),
    andCond(Val1,Val2,Condition).

eval_boolean(t_b_boolOr(X,Y),Env,NewEnv,Condition) :-
	eval_boolean(X,Env,Env1,Val1),eval_boolean(Y,Env1,NewEnv,Val2),
    orCond(Val1,Val2,Condition).

eval_boolean(t_b_true(),Env,Env,true).
eval_boolean(t_b_false(),Env,Env,false).

eval_boolean(t_b_not(X),Env,NewEnv,Condition) :- 
    eval_boolean(X,Env,NewEnv,Val1),not(Val1, Condition).

eval_boolean(t_b_equals(X,Y),Env,NewEnv,Condition) :- 
    eval_expr(X,Env,Val1,Env1), eval_expr(Y,Env1,Val2,NewEnv), 
    equal(Val1,Val2,Condition).

eval_boolean(t_b_equalsBool(_X,_Y),Env,Env,true).

eval_boolean(t_b_not_equals(X,Y),Env,NewEnv,Condition) :- 
    eval_expr(X,Env,Val1,Env1), eval_expr(Y,Env1,Val2,NewEnv), 
    equal(Val1,Val2,C1), not(C1,Condition).

eval_boolean(t_b_and(X,Y),Env,NewEnv,Condition) :- 
    eval_boolean(X,Env,Env1,Val1), eval_boolean(Y,Env1,NewEnv,Val2), 
    andCond(Val1,Val2,Condition).

eval_boolean(t_b_or(X,Y),Env,NewEnv,Condition) :- 
    eval_boolean(X,Env,Env1,Val1), eval_boolean(Y,Env1,NewEnv,Val2), 
    orCond(Val1, Val2, Condition).

eval_boolean(t_b_l(X,Y),Env,NewEnv,Condition) :- 
    eval_expr(X,Env,Val1,Env1), eval_expr(Y,Env1,Val2,NewEnv), 
    lesser(Val1,Val2,Condition).
eval_boolean(t_b_g(X,Y),Env,NewEnv,Condtition) :- 
    eval_expr(X,Env,Val1,Env1), eval_expr(Y,Env1,Val2,NewEnv), 
    greater(Val1,Val2,Condtition).
eval_boolean(t_b_lte(X,Y),Env,NewEnv,Condtition) :- 
    eval_expr(X,Env,Val1,Env1), eval_expr(Y,Env1,Val2,NewEnv), 
    lesserEqual(Val1,Val2,Condtition).
eval_boolean(t_b_gte(X,Y),Env,NewEnv,Condtition) :- 
    eval_expr(X,Env,Val1,Env1), eval_expr(Y,Env1,Val2,NewEnv),
    greaterEqual(Val1,Val2,Condtition).


not(true, false).
not(false,true).

equal(true, true, true).
equal(false, false, true).
equal(false, true, false).
equal(true, false, false).

equal(Val1, Val2, true):- number(Val1), number(Val2), Val1 is Val2.
equal(Val1, Val2, false):- number(Val1), number(Val2), \+ Val1 is Val2.

andCond(Val1,Val2,true):- Val1 = true, Val2 = true.
andCond(Val1,Val2,false):- Val1 = false , Val2 = false.
andCond(Val1,Val2,false):- Val1 = true , Val2 = false.
andCond(Val1,Val2,false):- Val1 = false , Val2 = true.

orCond(Val1,Val2, true):- Val1 = true, Val2 = true.
orCond(Val1,Val2, true):- Val1 = true, Val2 = false.
orCond(Val1,Val2, true):- Val1 = false, Val2 = true.
orCond(Val1,Val2, false):- Val1 = false, Val2 = false.

lesser(Val1,Val2,true):- Val1 < Val2.
lesser(Val1,Val2,false):- Val1 >= Val2.
greater(Val1,Val2,true):- Val1 > Val2.
greater(Val1,Val2,false):- Val1 =< Val2.

lesserEqual(Val1,Val2,true):- Val1 =< Val2.
lesserEqual(Val1,Val2,false):- Val1 > Val2.
greaterEqual(Val1,Val2,true):- Val1 >= Val2.
greaterEqual(Val1,Val2,false):- Val1 < Val2.

%--------------------------------------------------------------------------------
:- table expr/3, term/3.

assign(t_aBool(I,X)) --> word(I), [=], boolean(X).
assign(t_aAssign(I,Y)) --> word(I),[=], assign(Y).
assign(t_aInc(I)) --> word(I), [++].
assign(t_aDec(I)) --> word(I), [--].
assign(t_aAdd(I,X)) --> word(I), [+=], assign(X).
assign(t_aSub(I,X)) --> word(I), [-=], assign(X).
assign(t_aMult(I,X)) --> word(I), [*=], assign(X).
assign(t_aDiv(I,X)) --> word(I), [/=], assign(X).
assign(X) --> expr(X).

expr(t_add(X,Y)) --> expr(X),[+],term(Y).
expr(t_sub(X,Y)) --> expr(X),[-],term(Y).
expr(X) --> term(X).

term(t_mult(X,Y)) --> term(X),[*],paren(Y).
term(t_div(X,Y)) --> term(X),[/],paren(Y).
term(t_mod(X,Y)) --> term(X),['%'],paren(Y).
term(X) --> paren(X).

paren(t_paren(X)) --> ['('], assign(X), [')'].
paren(X) --> number(X) | string_q(X) | word(X).

% evaluate assignment statement
booleanCheck(t_b(Y)):- Y = true; Y = false.
    
eval_expr(t_aBool(t_word(I),Y), Env, Val, NewEnv) :- 
    eval_boolean(Y, Env,Env1, Val), update(I,Val,Env1,NewEnv).

eval_expr(t_aAssign(t_word(I),Y), Env, Val, NewEnv) :-
    \+ booleanCheck(t_b(Y)),
    eval_expr(Y, Env,Val, Env1), update(I,Val,Env1,NewEnv).


eval_expr(t_paren(X), Env, Val, NewEnv) :- eval_expr(X,Env, Val,NewEnv).

eval_expr(t_aInc(t_word(I)), Env, Val, NewEnv) :-
    eval_expr(t_add(t_word(I),t_num(1)), Env, Val, Env1), 
    update(I,Val,Env1,NewEnv).

eval_expr(t_aDec(t_word(I)), Env, Val, NewEnv) :-
    eval_expr(t_sub(t_word(I),t_num(1)), Env, Val, Env1), 
    update(I,Val,Env1,NewEnv).

eval_expr(t_aAdd(t_word(I),Y), Env, Val, NewEnv) :-
    eval_expr(t_add(t_word(I),Y), Env, Val, Env1),
    update(I,Val,Env1,NewEnv).

eval_expr(t_aSub(t_word(I),Y), Env, Val, NewEnv) :-
    eval_expr(t_sub(t_word(I),Y), Env, Val, Env1), 
    update(I,Val,Env1,NewEnv).

eval_expr(t_aMult(t_word(I),Y), Env, Val, NewEnv) :-
    eval_expr(t_mult(t_word(I),Y), Env, Val, Env1),
    update(I,Val,Env1,NewEnv).

eval_expr(t_aDiv(t_word(I),Y), Env, Val, NewEnv) :-
    eval_expr(t_div(t_word(I),Y), Env, Val, Env1),
    update(I,Val,Env1,NewEnv).

% evaluate addition, subtraction, division, multiplication
eval_expr(t_add(X,Y), Env, _Val, NewEnv):-
	eval_expr(X, Env, Val1, Env1), 
    eval_expr(Y, Env1, Val2, NewEnv), 
    number(Val1), string(Val2),
    writeln("int and string can not be concatenated"), fail.

eval_expr(t_add(X,Y), Env, _Val, NewEnv):-
	eval_expr(X, Env, Val1, Env1), 
    eval_expr(Y, Env1, Val2, NewEnv), 
    string(Val1), number(Val2),
    writeln("string and int can not be concatenated"), fail.


eval_expr(t_add(X,Y), Env, Val, NewEnv):-
	eval_expr(X, Env, Val1, Env1), 
    eval_expr(Y, Env1, Val2, NewEnv), 
    \+string(Val1), \+string(Val2),
    Val is Val1 + Val2.

eval_expr(t_add(X,Y), Env, Val, NewEnv):-
	eval_expr(X, Env, Val1, Env1), 
    eval_expr(Y, Env1, Val2, NewEnv), 
    \+ number(Val1), \+ number(Val2), 
    atomic_concat(Val1, Val2, Val).

eval_expr(t_sub(X,Y), Env, _Val, NewEnv):-
	eval_expr(X, Env, Val1, Env1),
    eval_expr(Y, Env1, Val2, NewEnv),
    string(Val1), string(Val2),
    writeln("string and string can not be subtrated"), fail.

eval_expr(t_sub(X,Y), Env, _Val, NewEnv):-
	eval_expr(X, Env, Val1, Env1),
    eval_expr(Y, Env1, Val2, NewEnv),
    string(Val1), number(Val2),
    writeln("string and int can not be subtrated"), fail.

eval_expr(t_sub(X,Y), Env, _Val, NewEnv):-
	eval_expr(X, Env, Val1, Env1),
    eval_expr(Y, Env1, Val2, NewEnv),
    number(Val1), string(Val2),
    writeln("int and string can not be subtrated"), fail.

eval_expr(t_sub(X,Y), Env, Val, NewEnv):-
	eval_expr(X, Env, Val1, Env1),
    eval_expr(Y, Env1, Val2, NewEnv),
    number(Val1), number(Val2),
    Val is Val1 - Val2.

eval_expr(t_mult(X,Y), Env, _Val, NewEnv):-
	eval_expr(X, Env, Val1, Env1),
    eval_expr(Y, Env1, Val2, NewEnv),
    string(Val1), string(Val2),
    writeln("string and string can not be multiplied"), fail.

eval_expr(t_mult(X,Y), Env, Val, NewEnv):-
	eval_expr(X, Env, Val1, Env1),
    eval_expr(Y, Env1, Val2, NewEnv),
    string(Val1), number(Val2),
    multiply_string(Val1, Val2, Val).

eval_expr(t_mult(X,Y), Env, Val, NewEnv):-
	eval_expr(X, Env, Val1, Env1),
    eval_expr(Y, Env1, Val2, NewEnv),
    number(Val1), string(Val2),
    multiply_string(Val2, Val1, Val).

eval_expr(t_mult(X,Y), Env, Val, NewEnv):-
eval_expr(X, Env, Val1, Env1),  
    eval_expr(Y, Env1, Val2, NewEnv), 
    number(Val1), number(Val2),
    Val is Val1 * Val2.

eval_expr(t_div(X,Y), Env, _Val, NewEnv):-
eval_expr(X, Env, Val1, Env1),
    eval_expr(Y, Env1, Val2, NewEnv),
    string(Val1), string(Val2),
    writeln("string and string can not be divided"), fail.

eval_expr(t_div(X,Y), Env, _Val, NewEnv):-
eval_expr(X, Env, Val1, Env1),
    eval_expr(Y, Env1, Val2, NewEnv),
    string(Val1), number(Val2),
    writeln("string and int can not be divided"), fail.

eval_expr(t_div(X,Y), Env, _Val, NewEnv):-
eval_expr(X, Env, Val1, Env1),
    eval_expr(Y, Env1, Val2, NewEnv),
    number(Val1), string(Val2),
    writeln("int and string can not be divided"), fail.

eval_expr(t_div(X,Y), Env, Val, NewEnv):-
eval_expr(X, Env, Val1, Env1), 
    eval_expr(Y, Env1, Val2, NewEnv),
    number(Val1), number(Val2),
    Val is Val1 / Val2.

eval_expr(t_mod(X,Y), Env, _Val, NewEnv):-
eval_expr(X, Env, Val1, Env1),
    eval_expr(Y, Env1, Val2, NewEnv),
    string(Val1), string(Val2),
    writeln("modulus of string and int can not be evaluated"), fail.

eval_expr(t_mod(X,Y), Env, _Val, NewEnv):-
eval_expr(X, Env, Val1, Env1),
    eval_expr(Y, Env1, Val2, NewEnv),
    string(Val1), number(Val2),
    writeln("modulus of string and int can not be evaluated"), fail.

eval_expr(t_mod(X,Y), Env, _Val, NewEnv):-
eval_expr(X, Env, Val1, Env1),
    eval_expr(Y, Env1, Val2, NewEnv),
    number(Val1), string(Val2),
    writeln("modulus of string and int can not be evaluated"), fail.

eval_expr(t_mod(X,Y), Env, Val, NewEnv):-
eval_expr(X, Env, Val1, Env1), 
    eval_expr(Y, Env1, Val2, NewEnv), 
    number(Val1), number(Val2),
    Val is Val1 mod Val2.

eval_expr(t_bool(I), Env, I, Env).
eval_expr(t_word(I), Env, Val, Env):- lookup(I, Env, Val).
eval_expr(t_num(X), Env, X, Env).
eval_expr(t_string(X), Env, X, Env).

% look for the variable value in the environment
lookup(I, [(I,Val)|_], Val).
lookup(I, [_|T], Val) :- lookup(I, T, Val).

% update value of variable in the current environment
update(Id, Val, [], [(Id, Val)]).
update(Id, Val, [(Id,_)|T], [(Id,Val)|T]).
update(Id, Val, [H|T], [H|R]) :-
       H \= (Id,_), update(Id, Val,T,R).


printseq(t_expr_print_ep(E,P))--> ['('],expr(E),[')'],[+], printseq(P).
printseq(t_expr_print_pe(P,E))--> [P], {string(P)}, [+], ['('],expr(E),[')'].
printseq(t_expr_print_pez(P,E,Z))--> [P], {string(P)}, [+], ['('],expr(E),[')'],[+], printseq(Z).
printseq(t_expr_print_e(E)) --> 
    \+printseq(t_expr_print_ep(_X,_Y)),
    \+printseq(t_expr_print_pe(_Z,_T)),
    expr(E).

eval_printseq(t_expr_print_ep(E,Ps),Env,NewEnv,Val) :-
    eval_expr(E,Env,Val1,Env1), eval_printseq(Ps,Env1,NewEnv,Val2),
    atomic_concat(Val1, Val2, Val).

eval_printseq(t_expr_print_pe(Ps,E),Env,NewEnv,Val) :-
    eval_expr(E,Env,Val1,NewEnv),
    atomic_concat(Ps, Val1, Val).

eval_printseq(t_expr_print_pez(P,E,Z),Env,NewEnv,Val) :-
   	eval_expr(E,Env,Val1,Env1), 
    atomic_concat(P, Val1, Val2),
    eval_printseq(Z,Env1,NewEnv,Val3),
    atomic_concat(Val2, Val3, Val).

eval_printseq(t_expr_print_e(E),Env,NewEnv,Val):- eval_expr(E,Env,Val,NewEnv).
%-------------------------------------------------------------------------
multiply_string(Val1, 1, Val1). 
multiply_string(Val1, N, Val) :-  
    N > 1, N1 is N - 1, multiply_string(Val1, N1, Val2),
    atomic_concat(Val1, Val2, Val).
multiply_string(_Val1, N, "") :-  
    N =< 0.
%--------------------------------------------------------------------------
string_q(t_string(X)) --> [X],{ string(X)}.
bool_keywords([true, false]).
bool(t_bool(X)) --> [X] , {bool_keywords(BK), member(X,BK)}.
keywords([+,-,>,<,=,while,for,if,elif,else,print,true,false]).
number(t_num(X)) --> [X],{number(X)}.
word(t_word(X)) --> [X],{atom(X),keywords(K),\+member(X,K)}.
