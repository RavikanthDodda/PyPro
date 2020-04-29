from sly import Lexer



class Lexer(Lexer):
    err = False
    # Set of token names.
    tokens   = {  IN, IF, ELSE, ELIF, WHILE, FOR, FALSE, TRUE, PRINT, RANGE, ID, NUMBER, FNUMBER, STRING, PLUS, MINUS, TIMES, MODULO,
               DIVIDE, IDIVIDE, POW, ASSIGN, PASSIGN, MASSIGN, DASSIGN, IDASSIGN, TASSIGN, RASSIGN,  EQUAL, LE, GE, GT, LT, NOT, INC, DEC, COMMA  }
    literals = { ';' ,'?', ':','!'}
    # Ignoring spaces, new lines and comments
    ignore = ' \t'
    ignore_multicomment = r'\$([^$]*\n?)*\$'
    ignore_newline = r'\n+'
    ignore_comment = r'\#(.*)'

    # Regular expression rules for tokens
    FNUMBER = r'\d+\.\d+'
    NUMBER  = r'\d+'
    EQUAL   = r'=='
    PASSIGN = r'\+='
    MASSIGN = r'-='
    IDASSIGN= r'//='
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
    IDIVIDE = r'/{2}'
    DIVIDE  = r'/'
    MODULO  = r'%'
    POW     = r'\^'
    
    BRACE   = r'(\(|\)|\{|\})'
    COMMA   = r','
    
    STRING      = r'"[^\"]*"'
    SSTRING     = r'\'[^\']*\''
    ID          = r'[a-zA-Z_][a-zA-Z0-9_]*'
    ID['if']    = IF
    ID['else']  = ELSE
    ID['elif']  = ELIF
    ID['while'] = WHILE
    ID['for']   = FOR
    ID['print'] = PRINT
    ID['range'] = RANGE
    ID['true']  = TRUE
    ID['false'] = FALSE
    ID['not']   = NOT
    ID['range'] = RANGE
    ID['in']    = IN
   
    # Additional operations on generated tokens
    def ignore_newline(self, t):
        self.lineno += len(t.value)

    def BRACE(self,t):
        t.value = "'"+t.value+"'" 
        return t

    def COMMA(self,t):
        t.value = "'"+t.value+"'" 
        return t

    def MODULO(self,t):
        t.value = "'"+t.value+"'" 
        return t

    def RASSIGN(self,t):
        t.value = "'"+t.value+"'" 
        return t
    
    def SSTRING(self,t):
        t.value = '"'+t.value[1:-1]+'"'
        return t
    
    def error(self, t):
        print("Invalid character '%s' at line: %d" % (t.value[0], self.lineno))
        self.index += 1
        self.err= True
