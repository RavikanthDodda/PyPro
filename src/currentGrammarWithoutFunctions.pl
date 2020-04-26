program(P) --> commands(P).

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

booleanCheck(t_b(Y)):- Y = true; Y = false.

printseq(t_expr_print_ep(E,P))--> ['('],expr(E),[')'],[+], printseq(P).
printseq(t_expr_print_pe(P,E))--> [P], {string(P)}, [+], ['('],expr(E),[')'].
printseq(t_expr_print_pez(P,E,Z))--> [P], {string(P)}, [+], ['('],expr(E),[')'],[+], printseq(Z).
printseq(t_expr_print_e(E)) --> 
    \+printseq(t_expr_print_ep(_X,_Y)),
    \+printseq(t_expr_print_pe(_Z,_T)),
    expr(E).

string_q(t_string(X)) --> [X],{ string(X)}.
bool_keywords([true, false]).
bool(t_bool(X)) --> [X] , {bool_keywords(BK), member(X,BK)}.
keywords([+,-,>,<,=,while,for,if,elif,else,print,true,false]).
number(t_num(X)) --> [X],{number(X)}.
word(t_word(X)) --> [X],{atom(X),keywords(K),\+member(X,K)}.