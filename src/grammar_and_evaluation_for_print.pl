:- table expr1/3, term/3, eval/4.

% necessary grammar and evalutaion rules for print

command(t_print(X)) --> [print], ['('],printseq(X),[')'].

eval_comm(t_print(X),Env,NewEnv) :-eval_printseq(X, Env,NewEnv,Val),write(Val).

printseq(t_expr_printseq_print(E,P))-->['('], expr(E),[')'],[+], printseq(P).
printseq(t_expr_print(E)) --> expr(E).

eval_printseq(t_expr_printseq_print(E,Ps),Env,NewEnv,Val) :-
    eval(E,Env,Env1,Val1), eval_printseq(Ps,Env1,NewEnv,Val2),
    atomic_concat(Val1, Val2, Val).

eval_printseq(t_expr_print(E),Env,NewEnv,Val):- eval(E,Env,NewEnv,Val).

eval(t_string_q(X),Env,Env,Val) :- eval_string(X,Env,Val).

string_q(t_string(X)) --> [X],{ string(X)}.

eval_string(t_string(X),_Env,X).

expr(t_string_q(X)) --> string_q(X).

% end print

expr(t_iden_assg_expr(I,E)) --> iden(I),[:=],expr(E).
expr(t_iden_assg_expr(I,E))--> iden(I),[:=],expr1(E).
expr(t_expr1(E)) --> expr1(E).
expr1(t_add(X,Y)) --> expr1(X), [+], term(Y).
expr1(t_sub(X,Y)) --> expr1(X),[-], term(Y).
expr1(term(X)) --> term(X).

term(t_mult(X,Y)) --> term(X), [*], p(Y).
term(t_div(X,Y)) --> term(X), [/], p(Y).
term(t_p(X)) --> p(X).

p(num(X)) --> num(X).
p(I) --> iden(I).
p(t_paran(X)) --> ['('], expr(X), [')'].

num(t_num(X)) --> [X], {number(X)}.
iden(I) --> [I], {atom(I)}.


eval(t_iden_assg_expr(I,E),Env,NewEnv,Val) :-
    eval_expr1(E,Env,Val),
    update(I,Val,Env,NewEnv).
eval(t_iden_assg_expr(I,E),Env,NewEnv,Val) :- 
    eval(E,Env,Env1,Val),
    update(I,Val,Env1,NewEnv).

eval(t_expr1(E),Env,_NewEnv,Val) :- eval_expr1(E,Env,Val).

update(Id,Val,[],[(Id,Val)]).
update(Id,Val,[(Id,_)|T],[(Id,Val)|T]).
update(Id,Val,[H|T],[H|R]) :- 
    H \= (Id,_),update(Id,Val,T,R).


eval_expr1(t_add(X,Y), Env,Val) :- eval_expr1(X,Env,Val1), 
    eval_term(Y,Env,Val2), Val is Val1 + Val2.

eval_expr1(t_sub(X,Y), Env, Val) :- eval_expr1(X, Env, Val1), 
    eval_term(Y, Env, Val2), Val is Val1 - Val2.

eval_expr1(term(X), Env,Val) :- eval_term(X,Env,Val).


eval_term(t_mult(X,Y), Env, Val) :- eval_term(X, Env, Val1),
    eval_p(Y,Env,Val2), Val is Val1 * Val2.

eval_term(t_div(X,Y), Env, Val) :- eval_term(X,Env, Val1), 
    eval_p(Y, Env, Val2), Val is Val1 / Val2.

eval_term(t_p(X),Env,Val) :- eval_p(X,Env,Val).

eval_p(t_paran(X), Env, Val) :- eval(X, Env,_NewEnv,Val). 

eval_p(num(X),Env,Val) :- eval_num(X,Env,Val).

eval_p(I,Env,Val) :- eval_iden(I,Env,Val).

eval_iden(I, Env, Val):- lookup(I,Env,Val).

eval_num(t_num(X),_Env,X).

lookup(Id, [(Id, Val)|_], Val). 
lookup(Id,[_|T], Val) :- lookup(Id, T, Val).


