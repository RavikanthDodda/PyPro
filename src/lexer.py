import sys
from sly import Lexer
from pyswip import Prolog 


class Lexer(Lexer):
    # Set of token names.   This is always required
    tokens = {  IN, IF, ELSE, ELIF, WHILE, FOR, FALSE, TRUE, PRINT, RANGE, ID, NUMBER, STRING, PLUS, MINUS, TIMES, MODULO,
               DIVIDE, ASSIGN, PASSIGN, MASSIGN, DASSIGN, TASSIGN, RASSIGN,  EQUAL, LE, GE, GT, LT, NOT, INC, DEC, COMMA  }
    literals = { ';' ,'?'}
    # Ignoring spaces, new lines and comments
    ignore = ' \t'
    ignore_newline = r'\n+'
    ignore_comment = r'\#(.*)'

    # Regular expression rules for tokens

    NUMBER  = r'\d+'
    EQUAL   = r'=='
    PASSIGN = r'\+='
    MASSIGN = r'-='
    DASSIGN = r'/='
    TASSIGN = r'\*='
    RASSIGN = r'%='
    ASSIGN  = r'='
    LE      = r'<='
    LT      = r'<'
    GE      = r'>='
    GT      = r'>'
    INC     = r'\+{2}'
    DEC     = r'-{2}'
    PLUS    = r'\+'
    MINUS   = r'-'
    TIMES   = r'\*'
    DIVIDE  = r'/'
    MODULO  = r'%'
    
    BRACE   = r'(\(|\)|\{|\})'
    COMMA   = r','
    
    STRING = r'"[^\"]*"'
    ID      = r'[a-zA-Z_][a-zA-Z0-9_]*'
    ID['if'] = IF
    ID['else'] = ELSE
    ID['elif'] = ELIF
    ID['while'] = WHILE
    ID['for'] = FOR
    ID['print'] = PRINT
    ID['range'] = RANGE
    ID['true']  = TRUE
    ID['false'] = FALSE
    ID['not'] = NOT
    ID['range'] = RANGE
    ID['in'] = IN
   
  
    def ignore_newline(self, t):
        self.lineno += len(t.value)

    def BRACE(self,t):
        t.value = "'"+t.value+"'" 
        return t

    def COMMA(self,t):
        t.value = "'"+t.value+"'" 
        return t
    
    def error(self, t):
        print("Invalid character '%s' at line: %d" % (t.value[0], self.lineno))
        self.index += 1

if __name__ == '__main__':
    inp_file = open(sys.argv[1],"r")
    data = inp_file.read()

    lexer = Lexer()
    tokenlist = "["

    for tok in lexer.tokenize(data):
        tokenlist += tok.value + "," 
    
    tokenlist = tokenlist[:-1]
    tokenlist +=  "]"

    prolog = Prolog() 
    prolog.consult('DCGwithEval.pl')
    t = list(prolog.query("program(P,"+tokenlist+", []),eval_program(P)"))
    if not bool(t):
        print("Runtime Error: Please check your code")