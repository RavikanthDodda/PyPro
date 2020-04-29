import sys
from pyswip import Prolog
from modules.lexer import Lexer

# Checking file extension
file_ext = sys.argv[1][-3:]
if file_ext == ".pr":
    # Handling file operations and file errors
    try:
        with open(sys.argv[1], "r") as inp_file:
            data = inp_file.read()
            # Getting lexer
            lexer = Lexer()
            tokenlist = "["
            # Generating tokenlist
            for tok in lexer.tokenize(data):
                tokenlist += tok.value + ","

            tokenlist = tokenlist[:-1]
            tokenlist += "]"
            if tokenlist == "]":
                tokenlist = "[]"
            # Interfacing with prolog
            prolog = Prolog()
            prolog.consult('/usr/lib/pypro/modules/pyproDCG.pl')
            prolog.consult('/usr/lib/pypro/modules/pyproEval.pl')
            if not lexer.err:
                # Querying for parse tree and semantics evaluation by passing tokenlist
                ast = list(prolog.query(
                    "program(P,"+tokenlist+", []),eval_program(P)"))
                # Checking for syntax/runtime errors
                if not bool(ast) and tokenlist != "[]":
                    print("Syntax/Runtime Error: Please check the code")
    except FileNotFoundError:
        print("No such file in path:", sys.argv[1])
else:
    print("Invalid file :", sys.argv[1])
