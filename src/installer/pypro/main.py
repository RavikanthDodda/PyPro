import sys
from pyswip import Prolog
from modules.lexer import Lexer

inp_file = open(sys.argv[1],"r")
data = inp_file.read()
lexer = Lexer()
tokenlist = "["

for tok in lexer.tokenize(data):
    tokenlist += tok.value + "," 

tokenlist = tokenlist[:-1]
tokenlist +=  "]"
if tokenlist=="]":
    tokenlist = "[]"
prolog = Prolog() 
prolog.consult('/usr/local/lib/pypro/modules/DCGwithEval.pl')
if not lexer.err:
    t = list(prolog.query("program(P,"+tokenlist+", []),eval_program(P)"))
    if not bool(t) and tokenlist!="[]":
        print("Runtime Error: Please check your code")