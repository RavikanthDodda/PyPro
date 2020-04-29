% program DCG takes list of tokens as inputs and provides parse tree as output.  
program(P) --> commands(P).

% commands DCG will parse multiple commands like variable creation, 
% variable assignment, if, if then else, if then elif then else, nested
% if else, for loop, while loop, ternary operator, funtion declaration,
% function call, print statement.
commands(t_command(X,Y)) --> command(X), commands(Y).
commands(t_command(X)) --> command(X).

command(t_command_assign(Y)) --> assign(Y), [;].

command(t_command_while(X,Y)) --> 
    [while], booleanBool(X), ['{'], commands(Y), ['}'].
command(t_command_while(X,Y)) --> 
    [while], ['('], booleanBool(X), [')'], ['{'], commands(Y), ['}'].

command(t_command_ternary(I,X,E1,E2)) -->
    word(I),[=], booleanBool(X),[?],expr(E1),[:],expr(E2),[;].
command(t_command_ternary(I,X,E1,E2)) -->
    word(I),[=], ['('], booleanBool(X), [')'],
    [?],expr(E1),[:],expr(E2),[;].

command(t_command_if(X,Y)) --> 
    [if], booleanBool(X), ['{'], commands(Y), ['}'].
command(t_command_ifel(X,Y,Z)) --> 
    [if], booleanBool(X), ['{'], commands(Y), ['}'],
    command_el(Z).

command(t_command_if(X,Y)) --> 
    [if], ['('], booleanBool(X), [')'], ['{'], commands(Y), ['}'].
command(t_command_ifel(X,Y,Z)) --> 
    [if], ['('], booleanBool(X), [')'] ,['{'], commands(Y), ['}'], 
    command_el(Z).

command(t_command_for_range(X,Y,Z,T))--> 
    [for], word(X), [in], [range],['('],expr(Y),[','],expr(Z),[')'],
    ['{'],commands(T),['}'].
command(t_command_for(X,Y,Z,T)) --> 
    [for], ['('],assign(X),[;], booleanBool(Y),
    [;],assign(Z),[')'], ['{'],commands(T),['}'].

command(t_command_for(X,Y,Z,T)) --> 
    [for], ['('],assign(X),[;], ['('], booleanBool(Y), [')'], 
    [;],assign(Z),[')'], ['{'],commands(T),['}'].

command(t_method_decl(X,Y,Z)) --> 
    [function], word(X),['('], parameterList(Y), [')'], 
    ['{'], commands(Z), ['}'].

command(t_method_decl_ret(X,Y,Z,E)) --> 
    [function],[return], word(X),['('], parameterList(Y), [')'], 
    ['{'], commands(Z), [return], expr(E),[;],['}'].
command(t_method_decl_ret(X,Y,E)) --> 
    [function],[return], word(X),['('], parameterList(Y), [')'], 
    ['{'], [return], expr(E),[;],['}'].

command(t_method_call(X,Y)) --> 
    word(X),['('], parameterList_call(Y), [')'],[;].

command(t_print(X)) --> 
    [print], ['('],printseq(X),[')'],[;].

command_el(t_command_el(X,Y)) --> 
    [elif], booleanBool(X), ['{'], commands(Y), ['}'].
command_el(t_command_el(X,Y,Z)) --> 
    [elif], booleanBool(X), ['{'], commands(Y), ['}'], 
    command_el(Z). 
command_el(t_command_else(Y)) --> 
    [else], ['{'], commands(Y), ['}'].

command_el(t_command_el(X,Y)) --> 
    [elif], ['('], booleanBool(X), [')'], ['{'], commands(Y), ['}'].
command_el(t_command_el(X,Y,Z)) --> 
    [elif], ['('], booleanBool(X), [')'], ['{'], commands(Y), ['}'], 
    command_el(Z).

% handles the print operation.
printseq(t_expr_print_ep(E,P))--> 
    ['('],expr(E),[')'],[+], printseq(P).
printseq(t_expr_print_sp(S,P))--> 
    [S], {string(S)}, [+], printseq(P).
printseq(t_expr_print_e(E)) --> 
    \+printseq(t_expr_print_ep(_,_)),
    \+printseq(t_expr_print_sp(_,_)),
    expr(E).

% parameterList DCG handles list of parameters that can be passed as arguments,
% during function declaration.
parameterList(t_parameter(X,Y)) --> 
    word(X),[','], parameterList(Y).
parameterList(X) --> word(X).

% parameterList_call DCG handles list of parameters that can be passed as arguments,
% during function call.
parameterList_call(t_parameter_call(X,Y)) --> 
    parameter_call(X), [','], parameterList_call(Y).
parameterList_call(t_parameter_call(X)) --> 
    parameter_call(X).
parameter_call(X) --> number(X) | word(X).

% boolean DCG handles all the boolean syntax while parsing.
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
boolean(t_b_equalsBool(true, false)) --> [true], [==], [false].
boolean(t_b_equalsBool(false, true)) --> [false], [==], [true].
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

booleanBool(t_b_boolNot(X)) --> [not], booleanBool(X).

booleanBool(t_b_boolAnd(X, Y)) --> boolean(X), [and], booleanTerm(Y).
booleanBool(t_b_boolAnd(X,Y)) --> booleanTerm(X), [and], boolean(Y).
booleanBool(t_b_boolOr(X, Y)) --> boolean(X), [or], booleanTerm(Y).
booleanBool(t_b_boolOr(X,Y)) --> booleanTerm(X), [or], boolean(Y).

booleanBool(t_b_boolAnd(X,Y)) --> booleanTerm(X), [and], booleanTerm(Y).
booleanBool(t_b_boolOr(X,Y)) --> booleanTerm(X), [or], booleanTerm(Y).

% assign DCG handles all the syntax related to expressions, such as addition,
% substraction, multiplication and many more. Here the grammar is right 
% associative.
:- table expr/3, term/3.

assign(t_aBool(I,X)) --> word(I), [=], boolean(X).
assign(t_aAssign(I,Y)) --> word(I),[=], assign(Y).
assign(t_aInc(I)) --> word(I), [++].
assign(t_aDec(I)) --> word(I), [--].
assign(t_aAdd(I,X)) --> word(I), [+=], assign(X).
assign(t_aSub(I,X)) --> word(I), [-=], assign(X).
assign(t_aMult(I,X)) --> word(I), [*=], assign(X).
assign(t_aDiv(I,X)) --> word(I), [/=], assign(X).
assign(t_aIDiv(I,X)) --> word(I), [//=], assign(X).
assign(t_aMod(X,Y)) --> word(X),['%='],assign(Y).
assign(X) --> expr(X).

expr(t_add(X,Y)) --> expr(X),[+],term(Y).
expr(t_sub(X,Y)) --> expr(X),[-],term(Y).
expr(X) --> term(X).

term(t_mult(X,Y)) --> term(X),[*],pow(Y).
term(t_div(X,Y)) --> term(X),[/],pow(Y).
term(t_idiv(X,Y)) --> term(X),[//],pow(Y).
term(t_mod(X,Y)) --> term(X),['%'],pow(Y).
term(X) --> pow(X).

pow(t_pow(X,Y)) --> uminus(X),[^],pow(Y).
pow(X) --> uminus(X).

uminus(t_umin(X)) --> [-],paren(X).
uminus(X) --> paren(X).

paren(t_paren(X)) --> ['('], assign(X), [')'].
paren(X) --> number(X) | string_q(X) | word(X).
paren(t_method_call_ret(X,Y)) --> [evaluate],word(X),['('], parameterList_call(Y), [')'].

% checks if the token is a list. 
string_q(t_string(X)) --> [X],{ string(X)}.

% checks if the given token is a number.
number(t_num(X)) --> [X],{number(X)}.

% checks id the given token is a word.
word(t_word(X)) --> [X],{atom(X),keywords(K),\+member(X,K)}.

% the list of all the keywords used by the pypro language.
keywords([+,-,>,<,=,while,for,if,elif,else,print,true,false,function,return,and,or,not,range,in]).
