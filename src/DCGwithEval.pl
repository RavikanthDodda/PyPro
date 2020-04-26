program(P) --> commands(P).

eval_program(P) :- eval_command(P,[],_).

%--------------------------------------------------------------------------------
commands(t_command(X,Y)) --> command(X), commands(Y).
commands(t_command(X)) --> command(X).

command(t_command_assign(Y)) --> assign(Y), [;].

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

boolean(t_b_Eequals(X,Y)) --> expr(X), [==], boolean(Y).
boolean(t_b_EnotEquals(X,Y)) --> expr(X), [!],[=], boolean(Y).
boolean(t_b_Bequals(X,Y)) --> boolean(X), [==], expr(Y).
boolean(t_b_BnotEquals(X,Y)) --> boolean(X), [!], [=], expr(Y).

boolean(t_b_equalsBool(true, true)) --> [true], [==], [true].
boolean(t_b_equalsBool(false, false)) --> [false], [==], [false].
boolean(t_b_not_equals(X,Y)) --> expr(X), [!], [=], expr(Y).

boolean(t_b_and(X,Y)) --> boolean(X),[and],boolean(Y).
boolean(t_b_or(X,Y)) --> boolean(X),[or],boolean(Y).
boolean(t_b_l(X,Y)) --> expr(X), [<], expr(Y).
boolean(t_b_g(X,Y)) --> expr(X), [>], expr(Y).
boolean(t_b_lte(X,Y)) --> expr(X), [<=], expr(Y).
boolean(t_b_gte(X,Y)) --> expr(X), [>=], expr(Y).

booleanTerm(t_b_num(X)) --> number(X).
booleanTerm(t_b_word(X)) --> word(X).
booleanTerm(t_b_string(X)) --> string_q(X).

booleanBool(X) --> boolean(X).
booleanBool(X) --> booleanTerm(X).

booleanBool(t_b_boolNot(X)) --> [not], booleanTerm(X).

booleanBool(t_b_boolAnd(X, Y)) --> boolean(X), [and], booleanTerm(Y).
booleanBool(t_b_boolAnd(X,Y)) --> booleanTerm(X), [and], boolean(Y).
booleanBool(t_b_boolOr(X, Y)) --> boolean(X), [or], booleanTerm(Y).
booleanBool(t_b_boolOr(X,Y)) --> booleanTerm(X), [or], boolean(Y).

booleanBool(t_b_boolAnd(X,Y)) --> booleanTerm(X), [and], booleanTerm(Y).
booleanBool(t_b_boolOr(X,Y)) --> booleanTerm(X), [or], booleanTerm(Y).

eval_boolean(t_b_string(X), Env,NewEnv, Condition) :-
    eval_expr(X,Env,Val1,NewEnv), equal(Val1, "", Val2), not(Val2,Condition).

eval_boolean(t_b_num(X), Env,NewEnv, Condition) :-
    eval_expr(X,Env,Val1,NewEnv), equal(Val1, 0, Val2), not(Val2,Condition).

eval_boolean(t_b_word(X),Env,NewEnv,Condition) :-
	eval_expr(X,Env,Condition,NewEnv).

eval_boolean(t_b_boolNot(X),Env,NewEnv,Condition) :-
	eval_boolean(X,Env,NewEnv,Val1), not(Val1, Condition).

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

eval_boolean(t_b_Eequals(X,Y),Env,NewEnv,Condition) :- 
    eval_expr(X,Env,Val1,Env1), eval_boolean(Y,Env1,NewEnv,Val2), 
    equal(Val1,Val2,Condition).

eval_boolean(t_b_Enotequals(X,Y),Env,NewEnv,Condition) :- 
    eval_expr(X,Env,Val1,Env1), eval_boolean(Y,Env1,NewEnv,Val2), 
    equal(Val1,Val2,V), not(V,Condition).

eval_boolean(t_b_Bequals(X,Y),Env,NewEnv,Condition) :- 
    eval_boolean(X,Env,Env1,Val1), eval_expr(Y,Env1,Val2,NewEnv),
    equal(Val1,Val2,Condition).

eval_boolean(t_b_Bnotequals(X,Y),Env,NewEnv,Condition) :- 
    eval_boolean(X,Env,Env1,Val1), eval_expr(Y,Env1,Val2,NewEnv),
    equal(Val1,Val2,V), not(V,Condition).

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

equal(Val1, Val2, true):- string(Val1), string(Val2), Val1 = Val2.
equal(Val1, Val2, false):- string(Val1), string(Val2), \+ Val1 = Val2.

equal(Val1, Val2, true):- number(Val1), number(Val2), Val1 is Val2.
equal(Val1, Val2, false):- number(Val1), number(Val2), \+ Val1 is Val2.

equal(Val1, Val2, false):- number(Val1), bool_keywords(Val2), 
    writeln("Number and boolean can not be compared."), fail.

equal(Val1, Val2, false):- string(Val1), bool_keywords(Val2), 
    writeln("String and boolean can not be compared.").
