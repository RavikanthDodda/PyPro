

:- table  expr/3, term/3.

%-------------------------------------

program(t_program(P))--> p(P).

p(t_p(B)) --> block(B).

% block part
block(t_block(D,B)) --> declarations(D),block(B).
block(t_block(C,B)) -->  commands(C),block(B).
block(t_block(D)) --> declaration(D).
block(t_block(C)) --> command(C).


% declarations part
declarations(t_decls(D1,D2)) -->   declaration(D1), declarations(D2). 
declarations(t_decls(D)) --> declaration(D).
declaration(t_decl(I,=,E,;)) -->  name(I), [=], expr(E), [;].
declaration(t_decl(I,;)) -->  name(I), [;].


% Commnads part
commands(t_commands(C1,C2)) -->   command(C1), commands(C2).
commands(t_commands(C)) -->   command(C).
command(t_command(I,=,Bl,?,E1:E2)) -->   name(I), [=], boolean(Bl), [?], expr(E1), :, expr(E2), [;].
% while part
command(t_command(while,Bl,'{',B,'}')) --> [while],boolean(Bl),['{'],block(B),['}'].
% if elif else part
% if part
command(t_command(if,Bl,'{',B,'}',Ce)) --> [if],boolean(Bl),['{'],block(B),['}'],command_el(Ce).
command(t_command(if,Bl,'{',B,'}')) --> [if],boolean(Bl),['{'],block(B),['}'].
% elif else part
command_el(t_command(elif,Bl,'{',B,'}',Ce)) --> [elif],boolean(Bl),['{'],block(B),['}'],command_el(Ce).
command_el(t_command(else,'{',B,'}')) --> [else],['{'],block(B),['}'].
% other commands
%command(t_command(A),;) -->   assign(A),[;].
%command(t_command(PrintSeq,;)) -->   print(PrintSeq), [;].
%command() →  MethodName(Call_Parameters), [;].
%command() →  Name = MethodName(Call_Parameters), [;].

% Conditional part
boolean(t_bool(E1,==,E2)) --> expr(E1),[==],expr(E2).
boolean(t_bool(E1,and,E2)) --> expr(E1),[and],expr(E2).
boolean(t_bool(E1,or,E2)) --> expr(E1),[or],expr(E2).
boolean(t_bool(not,Bl)) --> [not],boolean(Bl).
boolean(t_bool(true)) --> [true].
boolean(t_bool(false)) --> [false].

% expressions part
expr(t_e(X,+,Y)) --> expr(X),[+],term(Y).
expr(t_e(X,-,Y)) --> expr(X),[-],term(Y).
expr(X) --> term(X).

term(t_e(X,*,Y)) --> term(X),[*],num(Y).
term(t_e(X,/,Y)) --> term(X),[/],num(Y).
term(t_e(X,'%',Y)) --> term(X),['%'],num(Y).
term(X) --> num(X).

% numbers and variables rules
num(t_e('(',X,')')) --> ['('],expr(X),[')'].

num(X) --> name(X).
num(t_num(X)) --> [X],{number(X)}.

name(t_id(x)) --> [x].
name(t_id(y)) --> [y].
name(t_id(z)) --> [z].


%----------------------------------------------