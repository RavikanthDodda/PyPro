import sys
from pyswip import Prolog
from modules.lexer import Lexer

file_ext = sys.argv[1][-3:]
if file_ext == ".pr":
    try:
        with open(sys.argv[1], "r") as inp_file:
            data = inp_file.read()
            lexer = Lexer()
            tokenlist = "["

            for tok in lexer.tokenize(data):
                tokenlist += tok.value + ","

            tokenlist = tokenlist[:-1]
            tokenlist += "]"
            if tokenlist == "]":
                tokenlist = "[]"
            prolog = Prolog()
            prolog.consult('./modules/pyproDCG.pl')
            prolog.consult('./modules/pyproEval.pl')
            if not lexer.err:
                t = list(prolog.query(
                    "program(P,"+tokenlist+", []),eval_program(P)"))
                if not bool(t) and tokenlist != "[]":
                    print("Runtime Error: Please check the code")
    except FileNotFoundError:
        print("No such file in path:", sys.argv[1])
else:
    print("Invalid file :", sys.argv[1])
