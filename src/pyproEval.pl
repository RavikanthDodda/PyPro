% eval_prgram(ParseTree) takes parse tree generated by the parser and executes.
eval_program(P) :- eval_command(P,[],_).

% eval_command(ParseTree,Env,NewEnv) handles parse trees generated by Command DCG.
eval_command(t_command_assign(X),Env,NewEnv) :- 
    eval_expr(X,Env,_Val,NewEnv).

eval_command(t_command(X,Y),Env,NewEnv) :- 
    eval_command(X,Env,NewEnv1), 
    eval_command(Y,NewEnv1,NewEnv).
eval_command(t_command(X),Env,NewEnv) :- 
    eval_command(X,Env,NewEnv).

eval_command(t_command_ternary(t_word(I),X,E1,_E2),Env,NewEnv):-
    eval_boolean(X,Env,NewEnv1,true),
    eval_expr(E1,NewEnv1,Val,NewEnv2),
    update(I,Val,NewEnv2,NewEnv).
eval_command(t_command_ternary(t_word(I),X,_E1,E2),Env,NewEnv):-
    eval_boolean(X,Env,NewEnv1,false),
    eval_expr(E2,NewEnv1,Val,NewEnv2),
    update(I,Val,NewEnv2,NewEnv).
    
eval_command(t_command_if(X,Y),Env,NewEnv) :- 
    eval_boolean(X,Env,NewEnv1,true), 
    eval_command(Y,NewEnv1,NewEnv). 
eval_command(t_command_if(X,_Y),Env,Env) :- 
    eval_boolean(X,Env,Env,false). 

eval_command(t_command_ifel(X,Y,_Z),Env,NewEnv) :- 
    eval_boolean(X,Env,NewEnv1,true), 
    eval_command(Y,NewEnv1,NewEnv). 
eval_command(t_command_ifel(X,_Y,Z),Env,NewEnv) :- 
    eval_boolean(X,Env,NewEnv1,false),
    eval_command(Z,NewEnv1,NewEnv).

eval_command(t_command_el(X,Y,_Z),Env,NewEnv) :- 
    eval_boolean(X,Env,NewEnv1,true), 
    eval_command(Y,NewEnv1,NewEnv). 
eval_command(t_command_el(X,_Y,Z),Env,NewEnv) :- 
    eval_boolean(X,Env,NewEnv1,false),
    eval_command(Z,NewEnv1,NewEnv).
eval_command(t_command_el(X,Y),Env,NewEnv) :- 
    eval_boolean(X,Env,NewEnv1,true),
    eval_command(Y,NewEnv1,NewEnv).

eval_command(t_command_else(X),Env,NewEnv) :- 
   	eval_command(X,Env,NewEnv).

eval_command(t_command_while(X,Y),Env,NewEnv) :-  
    eval_boolean(X,Env,NewEnv1,true),
    eval_command(Y,NewEnv1,NewEnv2),
    eval_command(t_command_while(X,Y),NewEnv2,NewEnv).
eval_command(t_command_while(X,_Y),Env,NewEnv) :-  
    eval_boolean(X,Env,NewEnv,false).

eval_command(t_command_for_range(t_word(X),Y,Z,T),Env,NewEnv) :- 
    lookup(X,Env,Val1),
    eval_expr(Y,Env,Val2,Env1), eval_expr(Z,Env1,Val3,Env2), 
    (between(Val2, Val3, Val1)-> eval_command(T,Env2,Env3), 
    lookup(X,Env3,Val4), Val5 is Val4 + 1, update(X,Val5,Env3,Env4),
    eval_command(t_command_for_range(t_word(X),Y,Z,T),Env4,NewEnv);
    NewEnv = Env).

eval_command(t_command_for_range(t_word(X),Y,Z,T),Env,NewEnv) :-
    \+lookup(X,Env,_Val1),eval_expr(Y,Env,Val2,Env1),
    update(X,Val2,Env1,Env2), eval_expr(Z,Env2,_Val3,Env3), 
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
    eval_command(X,Env,NewEnv1),
    eval_boolean(Y,NewEnv1,NewEnv,false).  
eval_command(t_command_for(Y,_Z,_T), Env, NewEnv) :- 
    eval_boolean(Y,Env,NewEnv,false).

eval_command(t_print(X),Env,NewEnv) :- 
    eval_printseq(X, Env,NewEnv,Val),writeln(Val).

eval_command(t_method_decl(t_word(X),Y,Z),Env,NewEnv) :- 
    \+ lookup(X,Env,_Val), 
    update(X,t_method_decl(X,Y,Z),Env, NewEnv).
eval_command(t_method_decl(t_word(X),_Y,_Z),Env,Env) :-
    lookup(X,Env,_Val), 
    write(X),writeln(" function name is already defined."), fail.

eval_command(t_method_call(t_word(X),Y),Env,NewEnv) :- 
    lookup(X,Env,Method),
    eval_method(Method,Y,Env,NewEnv).

eval_command(t_method_decl_ret(t_word(X),Y,Z,E),Env,NewEnv) :- 
    \+lookup(X,Env,_Val), 
    update(X,t_method_decl_ret(X,Y,Z,E),Env, NewEnv).
eval_command(t_method_decl_ret(t_word(X),_Y,_Z,_E),Env,Env) :- 
    lookup(X,Env,_Val), 
    write(X),writeln(" function name is already defined."), fail.

eval_command(t_method_decl_ret(t_word(X),Y,E),Env,NewEnv) :- 
    \+lookup(X,Env,_Val), 
    update(X,t_method_decl_ret(X,Y,E),Env, NewEnv).
eval_command(t_method_decl_ret(t_word(X),_Y,_E),Env,Env) :- 
    lookup(X,Env,_Val), 
    write(X),writeln(" function name is already defined."), fail.

% eval_method(ParseTree, ParameterList, Environment, NewEnvironment) 
% handles method declaration
eval_method(t_method_decl(_X,Y,Z),U,Env,NewEnv) :- 
    eval_parameter(Y,L1) , eval_parameter_call(U,L2,Env,Env1),
    length(L1,Len1), length(L2,Len2), 
    Len1 = Len2, assignParam(L1,L2,Env1,Env2),
    eval_command(Z,Env2,NewEnv).

eval_method(t_method_decl(X,Y,_Z),U,Env,NewEnv) :- 
    eval_parameter(Y,L1) , eval_parameter_call(U,L2,Env,NewEnv),
    length(L1,Len1), length(L2,Len2), Len1 \= Len2,
    write("parameters does not match for function call "), 
    writeln(X), fail.

eval_method(t_method_decl_ret(_X,Y,Z,E),U,Env,Val,NewEnv):-
    eval_parameter(Y,L1) , eval_parameter_call(U,L2,Env,Env1),
    length(L1,Len1), length(L2,Len2), 
	Len1 = Len2, assignParam(L1,L2,Env1,Env2),
    eval_command(Z,Env2,Env3), eval_expr(E,Env3,Val,NewEnv).

eval_method(t_method_decl_ret(X,Y,_Z,_E),U,Env,_Val,_NewEnv):-
    eval_parameter(Y,L1) , eval_parameter_call(U,L2,Env,_Env1),
    length(L1,Len1), length(L2,Len2),  Len1 \= Len2,
    write("parameters does not match for function call "), 
    writeln(X), fail.

eval_method(t_method_decl_ret(_X,Y,E),U,Env,Val,NewEnv):-
    eval_parameter(Y,L1) , eval_parameter_call(U,L2,Env,Env1),
    length(L1,Len1), length(L2,Len2), 
	Len1 = Len2, assignParam(L1,L2,Env1,Env2),
    eval_expr(E,Env2,Val,NewEnv).

eval_method(t_method_decl_ret(X,Y,_E),U,Env,_Val,_NewEnv):-
    eval_parameter(Y,L1) , eval_parameter_call(U,L2,Env,_Env1),
    length(L1,Len1), length(L2,Len2),  Len1 \= Len2,
    write("parameters does not match for function call "), 
    writeln(X), fail.

% assignParam(ParameterList,ParameterList_call,Environment,Environment)
% assigns values of called parameters to declared parameters.
assignParam([],[],Env,Env).
assignParam([H1|T1],[H2|T2],Env,NewEnv) :-  
    update(H1,H2,Env,Env1), 
    assignParam(T1,T2,Env1,NewEnv).

% eval_parameter(ParseTree, List of Variable names) returns 
% list variable names of the declared parameter.
eval_parameter(t_word(X),[X]).
eval_parameter(t_parameter(t_word(X),Y),[X|R]) :- eval_parameter(Y,R).

% eval_parameter(ParseTree, List of values) returns 
% list of values of variables of the called parameter.
eval_parameter_call(t_parameter_call(X),[R],Env,NewEnv) :- 
    eval_expr(X,Env,R,NewEnv).
eval_parameter_call(t_parameter_call(X,Y),[R1|R],Env,NewEnv) :- 
    eval_expr(X,Env,R1,Env1), eval_parameter_call(Y,R,Env1,NewEnv).

% eval_boolean(ParseTree, Env, NewEnv, Condition) checks if a condition is
% true or false.
eval_boolean(t_b_string(X), Env,NewEnv, Condition) :-
    eval_expr(X,Env,Val1,NewEnv), 
    equal(Val1, "", Val2), not(Val2,Condition).

eval_boolean(t_b_num(X), Env,NewEnv, Condition) :-
    eval_expr(X,Env,Val1,NewEnv), 
    equal(Val1, 0, Val2), 
    not(Val2,Condition).

eval_boolean(t_b_word(X),Env,NewEnv,Condition) :-
	eval_expr(X,Env,Val1,NewEnv), 
    string(Val1), equal(Val1, "", Val2),
    not(Val2,Condition).

eval_boolean(t_b_word(X),Env,NewEnv,Condition) :-
	eval_expr(X,Env,Val1,NewEnv), 
    number(Val1), equal(Val1, 0, Val2),
    not(Val2,Condition).

eval_boolean(t_b_word(X),Env,NewEnv,Condition) :-
	eval_expr(X,Env,Condition,NewEnv),
    bool_keywords(Condition).

eval_boolean(t_b_boolNot(X),Env,NewEnv,Condition) :-
	eval_boolean(X,Env,NewEnv,Val1),
    not(Val1, Condition).

eval_boolean(t_b_boolAnd(X,Y),Env,NewEnv,Condition) :-
	eval_boolean(X,Env,Env1,Val1),
    eval_boolean(Y,Env1,NewEnv,Val2),
    andCond(Val1,Val2,Condition).

eval_boolean(t_b_boolOr(X,Y),Env,NewEnv,Condition) :-
	eval_boolean(X,Env,Env1,Val1),
    eval_boolean(Y,Env1,NewEnv,Val2),
    orCond(Val1,Val2,Condition).

eval_boolean(t_b_true(),Env,Env,true).
eval_boolean(t_b_false(),Env,Env,false).

eval_boolean(t_b_not(X),Env,NewEnv,Condition) :- 
    eval_boolean(X,Env,NewEnv,Val1),
    not(Val1, Condition).

eval_boolean(t_b_Eequals(X,Y),Env,NewEnv,Condition) :- 
    eval_expr(X,Env,Val1,Env1),
    eval_boolean(Y,Env1,NewEnv,Val2), 
    equal(Val1,Val2,Condition).

eval_boolean(t_b_Enotequals(X,Y),Env,NewEnv,Condition) :- 
    eval_expr(X,Env,Val1,Env1), 
    eval_boolean(Y,Env1,NewEnv,Val2), 
    equal(Val1,Val2,V), not(V,Condition).

eval_boolean(t_b_Bequals(X,Y),Env,NewEnv,Condition) :- 
    eval_boolean(X,Env,Env1,Val1), 
    eval_expr(Y,Env1,Val2,NewEnv),
    equal(Val1,Val2,Condition).

eval_boolean(t_b_Bnotequals(X,Y),Env,NewEnv,Condition) :- 
    eval_boolean(X,Env,Env1,Val1), 
    eval_expr(Y,Env1,Val2,NewEnv),
    equal(Val1,Val2,V),
    not(V,Condition).

eval_boolean(t_b_equals(X,Y),Env,NewEnv,Condition) :- 
    eval_expr(X,Env,Val1,Env1), 
    eval_expr(Y,Env1,Val2,NewEnv), 
    equal(Val1,Val2,Condition).

eval_boolean(t_b_equalsBool(X,Y),Env,Env,Condition) :- 
   equal(X,Y,Condition).

eval_boolean(t_b_not_equals(X,Y),Env,NewEnv,Condition) :- 
    eval_expr(X,Env,Val1,Env1), 
    eval_expr(Y,Env1,Val2,NewEnv), 
    equal(Val1,Val2,C1), 
    not(C1,Condition).

eval_boolean(t_b_and(X,Y),Env,NewEnv,Condition) :- 
    eval_boolean(X,Env,Env1,Val1), 
    eval_boolean(Y,Env1,NewEnv,Val2), 
    andCond(Val1,Val2,Condition).

eval_boolean(t_b_or(X,Y),Env,NewEnv,Condition) :- 
    eval_boolean(X,Env,Env1,Val1), 
    eval_boolean(Y,Env1,NewEnv,Val2), 
    orCond(Val1, Val2, Condition).

eval_boolean(t_b_l(X,Y),Env,NewEnv,Condition) :- 
    eval_expr(X,Env,Val1,Env1), 
    eval_expr(Y,Env1,Val2,NewEnv), 
    lesser(Val1,Val2,Condition).
eval_boolean(t_b_g(X,Y),Env,NewEnv,Condtition) :- 
    eval_expr(X,Env,Val1,Env1), 
    eval_expr(Y,Env1,Val2,NewEnv), 
    greater(Val1,Val2,Condtition).
eval_boolean(t_b_lte(X,Y),Env,NewEnv,Condtition) :- 
    eval_expr(X,Env,Val1,Env1),
    eval_expr(Y,Env1,Val2,NewEnv), 
    lesserEqual(Val1,Val2,Condtition).
eval_boolean(t_b_gte(X,Y),Env,NewEnv,Condtition) :- 
    eval_expr(X,Env,Val1,Env1), 
    eval_expr(Y,Env1,Val2,NewEnv),
    greaterEqual(Val1,Val2,Condtition).

% not(X,Result) returns true if X is false.
% returns false id X is true.
not(true, false).
not(false,true).

% equal(X,Y,Result) returns true if X and Y 
% are true, else false.
equal(true, true, true).
equal(false, false, true).
equal(false, true, false).
equal(true, false, false).

equal(Val1, Val2, true):- 
    string(Val1), string(Val2), Val1 = Val2.
equal(Val1, Val2, false):- 
    string(Val1), string(Val2), \+ Val1 = Val2.

equal(Val1, Val2, true):- 
    number(Val1), number(Val2), Val1 is Val2.
equal(Val1, Val2, false):- 
    number(Val1), number(Val2), \+ Val1 is Val2.

equal(Val1, Val2, false):- 
    number(Val1), bool_keywords(Val2), 
    writeln("Number and boolean can not be compared."), fail.
equal(Val1, Val2, false):- 
    string(Val1), bool_keywords(Val2), 
    writeln("String and boolean can not be compared."), fail.
equal(Val1, Val2, false):-  
    bool_keywords(Val1),number(Val2),
    writeln("Boolean and Number can not be compared."), fail.
equal(Val1, Val2, false):-  
    bool_keywords(Val1), string(Val2),
    writeln("Boolean and String can not be compared."), fail.

% andCond(X,Y,Result) returns true if X and Y are true, else false.
andCond(Val1,Val2,true):- Val1 = true, Val2 = true.
andCond(Val1,Val2,false):- Val1 = false , Val2 = false.
andCond(Val1,Val2,false):- Val1 = true , Val2 = false.
andCond(Val1,Val2,false):- Val1 = false , Val2 = true.

% orCond(X,Y,Result) returns true if X is true or Y is true, else false.
orCond(Val1,Val2, true):- Val1 = true, Val2 = true.
orCond(Val1,Val2, true):- Val1 = true, Val2 = false.
orCond(Val1,Val2, true):- Val1 = false, Val2 = true.
orCond(Val1,Val2, false):- Val1 = false, Val2 = false.

% lesser(X,Y,Result) returns true if X is less than Y, else false.
lesser(Val1,Val2,true):- Val1 < Val2.
lesser(Val1,Val2,false):- Val1 >= Val2.

% greater(X,Y,Result) returns true if X is greater than Y, else false.
greater(Val1,Val2,true):- Val1 > Val2.
greater(Val1,Val2,false):- Val1 =< Val2.

% lesserEqual(X,Y,Result) returns true if X is less than Y 
% or equal to Y then true, else false.
lesserEqual(Val1,Val2,true):- Val1 =< Val2.
lesserEqual(Val1,Val2,false):- Val1 > Val2.

% greaterEqual(X,Y,Result) returns true if X is greater than Y 
% or equal to Y then true, else false.
greaterEqual(Val1,Val2,true):- Val1 >= Val2.
greaterEqual(Val1,Val2,false):- Val1 < Val2.

% eval_expr(ParseTree,Env,Value,NewEnv) evaluates the expression and 
% returns the value and new environment.
eval_expr(t_aBool(t_word(I),Y), Env, Val, NewEnv) :- 
    eval_boolean(Y, Env,Env1, Val), 
    update(I,Val,Env1,NewEnv).

eval_expr(t_aAssign(t_word(I),Y), Env, Val, NewEnv) :-
    \+ booleanCheck(t_b(Y)),
    eval_expr(Y, Env,Val, Env1), 
    update(I,Val,Env1,NewEnv).
    
eval_expr(t_paren(X), Env, Val, NewEnv) :- 
    eval_expr(X,Env, Val,NewEnv).

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

eval_expr(t_aIDiv(t_word(I),Y), Env, Val, NewEnv) :-
    eval_expr(t_idiv(t_word(I),Y), Env, Val, Env1),
    update(I,Val,Env1,NewEnv).

eval_expr(t_aMod(t_word(I),Y), Env, Val, NewEnv) :-
    eval_expr(t_mod(t_word(I),Y), Env, Val, Env1),
    update(I,Val,Env1,NewEnv).

% evaluate addition, subtraction, division, multiplication
eval_expr(t_add(X,Y), Env, Val, NewEnv):-
	eval_expr(X, Env, Val1, Env1), 
    eval_expr(Y, Env1, Val2, NewEnv), 
    string(Val1), \+string(Val2),
    atomic_concat(Val1, Val2, Val) |
    eval_expr(X, Env, Val1, Env1), 
    eval_expr(Y, Env1, Val2, NewEnv), 
    \+string(Val1), string(Val2),
    atomic_concat(Val1, Val2, Val) | 
    eval_expr(X, Env, Val1, Env1), eval_expr(Y, Env1, Val2, NewEnv), 
    \+string(Val1), \+string(Val2),
    Val is Val1 + Val2 |
    eval_expr(X, Env, Val1, Env1), eval_expr(Y, Env1, Val2, NewEnv), 
    \+ number(Val1), \+ number(Val2), 
    atomic_concat(Val1, Val2, Val).

eval_expr(t_mult(X,Y), Env, _Val, NewEnv):-
	eval_expr(X, Env, Val1, Env1),
    eval_expr(Y, Env1, Val2, NewEnv),
    string(Val1), string(Val2),
    writeln("Type Error: multiplication of string and string is not allowed"), fail.

eval_expr(t_mult(X,Y), Env, Val, NewEnv):-
	eval_expr(X, Env, Val1, Env1),
    eval_expr(Y, Env1, Val2, NewEnv),
    string(Val1), number(Val2),
    multiply_string(Val1, Val2, Val) |
    
    eval_expr(X, Env, Val1, Env1),
    eval_expr(Y, Env1, Val2, NewEnv),
    number(Val1), string(Val2),
    multiply_string(Val2, Val1, Val) |
    
    eval_expr(X, Env, Val1, Env1),
    eval_expr(Y, Env1, Val2, NewEnv), 
    number(Val1), number(Val2),
    Val is Val1 * Val2.

eval_expr(t_sub(X,Y), Env, Val, NewEnv):-
	eval_expr(X, Env, Val1, Env1),
    eval_expr(Y, Env1, Val2, NewEnv),
    eval_type_check(Val1,Val2,subtraction,number,number),
    Val is Val1 - Val2.

eval_expr(t_div(X,Y), Env, Val, NewEnv):-
	eval_expr(X, Env, Val1, Env1), 
    eval_expr(Y, Env1, Val2, NewEnv),
    eval_type_check(Val1,Val2,divison,number,number),
    Val is Val1 / Val2.

eval_expr(t_idiv(X,Y), Env, Val, NewEnv):-
	eval_expr(X, Env, Val1, Env1), 
    eval_expr(Y, Env1, Val2, NewEnv),
    eval_type_check(Val1,Val2,int_divison,number,number),
    Val is Val1 // Val2.

eval_expr(t_mod(X,Y), Env, Val, NewEnv):-
	eval_expr(X, Env, Val1, Env1),  
    eval_expr(Y, Env1, Val2, NewEnv), 
    eval_type_check(Val1,Val2,modulus,number,number),
    Val is Val1 mod Val2.

eval_expr(t_pow(X,Y), Env, Val, NewEnv):-
    eval_expr(X, Env, Val1, Env1), 
    eval_expr(Y, Env1, Val2, NewEnv), 
    eval_type_check(Val1,Val2,power,number,number),
    Val is Val1 ^ Val2.

eval_expr(t_umin(X),Env,Val,NewEnv):-
    eval_expr(X,Env,Val1,NewEnv), 
    number(Val1) ,Val is -1 * Val1.

eval_expr(t_umin(X),Env,_Val,NewEnv):-
    eval_expr(X,Env,Val1,NewEnv), \+ number(Val1), 
	writeln("unary minus cannot be used with string."), fail.

eval_expr(t_method_call_ret(t_word(X),Y),Env,Val,NewEnv) :- 
    lookup(X,Env,Method),
    eval_method(Method,Y,Env,Val,NewEnv). 

eval_expr(t_bool(I), Env, I, Env).
eval_expr(t_num(X), Env, X, Env).
eval_expr(t_string(X), Env, X, Env).
eval_expr(t_word(I), Env, Val, Env):- lookup(I, Env, Val).

booleanCheck(t_b(Y)):- Y = true; Y = false.

% multiply_string(String, N, Result) concats the string N times 
% and returs as a result.
multiply_string(Val1, 1, Val1). 
multiply_string(Val1, N, Val) :-  
    N > 1, N1 is N - 1, multiply_string(Val1, N1, Val2),
    atomic_concat(Val1, Val2, Val).
multiply_string(_Val1, N, "") :-  
    N =< 0.

% eval_type_check(Value1, Value2, X,Y) evaluates whether the string and 
% number combinations are correct.
eval_type_check(Val1,Val2,_Op,string,number):- string(Val1),number(Val2). 
eval_type_check(Val1,Val2,_Op,number,string):- number(Val1),string(Val2). 
eval_type_check(Val1,Val2,_Op,number,number):- number(Val1),number(Val2). 
eval_type_check(Val1,Val2,_Op,string,string):- string(Val1),string(Val2). 

eval_type_check(Val1,Val2,Op,string,number):- 
    \+string(Val1),\+number(Val2),write("Type Error: "),
    write(Op),writeln(" of number and string is not allowed"), fail.
eval_type_check(Val1,Val2,Op,number,number):- 
    \+number(Val1),\+number(Val2),write("Type Error: "),
    write(Op),writeln(" of string and string is not allowed"), fail.
eval_type_check(Val1,Val2,Op,number,string):- 
    \+number(Val1),\+string(Val2),write("Type Error: "),
    write(Op),writeln(" of string and number is not allowed"), fail.
eval_type_check(Val1,Val2,Op,string,string):- 
    \+string(Val1),\+string(Val2),write("Type Error: "),
    write(Op),writeln(" of number and number is not allowed"), fail.

% eval_printseq(ParseTree,Env,NewEnv,Val) prints as a string, if any 
% variable or expression, evaluates the same and prints the result
% as a string.
eval_printseq(t_expr_print_ep(E,P),Env,NewEnv,Val) :-
    eval_expr(E,Env,Val1,Env1), eval_printseq(P,Env1,NewEnv,Val2),
    atomic_concat(Val1, Val2, Val).

eval_printseq(t_expr_print_sp(S,P),Env,NewEnv,Val) :-
    eval_printseq(P,Env,NewEnv,Val1),
    atomic_concat(S, Val1, Val).

eval_printseq(t_expr_print_e(E),Env,NewEnv,Val):- 
    eval_expr(E,Env,Val,NewEnv).

% look for the variable value in the environment
lookup(I, [(I,Val)|_], Val).
lookup(I, [_|T], Val) :- lookup(I, T, Val).

% update value of variable in the current environment
update(Id, Val, [], [(Id, Val)]).
update(Id, Val, [(Id,_)|T], [(Id,Val)|T]).
update(Id, Val, [H|T], [H|R]) :-
       H \= (Id,_), update(Id, Val,T,R).

bool_keywords(true).
bool_keywords(false).
