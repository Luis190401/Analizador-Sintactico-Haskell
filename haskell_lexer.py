import ply.lex as lex
import sys
from ply.lex import Lexer
# Chuc Balam Addiel Benjamin - 19070061
# Dzul Noh Jose Julio - 19070047
# Llanes Barrera Luis Angel - 19070023
# Poot Pool Riger Emmanuel - 19070067
# Velasco Gomez Luis Enrique - 19070037

reserved = {
    'Bool': 'BOOL',
    'case': 'CASE',
    'class': 'CLASS',
    'Char' : 'CHAR',
    'data': 'DATA',
    'deriving': 'DERIVING',
    'do': 'DO',
    'Double': 'DOUBLE',
    'div': 'DIV',
    'else': 'ELSE',
    'if': 'IF',
    'import': 'IMPORT',
    'in': 'IN',
    'Int': 'INT',
    'Interger' : 'INTERGER',
    'print': 'PRINT',
    'private': 'PRIVATE',
    'protected': 'PROTECTED',
    'public': 'PUBLIC',
    'require': 'REQUIRE',
    'return': 'RETURN',
    'static': 'STATIC',
    'switch': 'SWITCH',
    'String' : 'STRING',
    'this': 'THIS',
    'var': 'VAR',
    'while': 'WHILE',
    'Float' : 'FLOAT',
    'filter': 'FILTER',
    'infix':'INFIX',
    'infixl': 'INFIXL',
    'infixr': 'INFIXR',
    'instance': 'INSTANCE',
    'let': 'LET',
    'of': 'OF',
    'module': 'MODULE',
    'newtype': 'NEWTYPE',
    'then': 'THEN',
    'type': 'TYPE',
    'where': 'WHERE',
    'array': 'ARRAY',
    'break': 'BREAK',
    'extends': 'EXTENDS',
    'elseif': 'ELSEIF',
    'exit': 'EXIT',
    'fn': 'FN',
    'for': 'FOR',
    'function': 'FUNCTION',
    'show' : 'SHOW'
}

tokens = list(reserved.values()) + [
    # Symbols

    'ASSIGN',
    'MOD',
    'SUMA',
    'CONECTARLISTA1',
    'CONECTARLISTA2',
    'IGUALDAD',
    'IGUALDADES',
    'MENOS',
    'MULTIPLICAR',
    'DIVISION',
    'MENOSQUE',
    'MENOSOIGUAL',
    'MAYORQUE',
    'MAYOROIGUAL',
    'IGUAL',
    'PUNTOYCOMA',
    'DOBLEPUNTO',
    'PARENTESISIZ',
    'PARENTESISDE',
    'CORCHETEIZ',
    'CORCHETEDE',
    'LBLOCK',
    'RBLOCK',
    'COMA',
    'HASHTAG',
    'PUNTO',
    'QUESTIONMARK',
    'COMILLASIMPLE',
    'COMILLASDOBLES',
    'CONS',

    # variables

    # Others
    'VARIABLE',
    # 'VARIABLE2',
    'NUMBER',
    'CADENAVACIA',
    'CADENA1',
    # 'CADENA2',
    'ID',
    'COMENTARIOENLINEAS',
    'COMENTARIOSENLINEA',
]

# Regular expressions rules for simple tokens
# t_CONECTARLISTA = r'\++'
t_MOD = r'%'
t_SUMA = r'\+'
t_MENOS = r'-'
t_MULTIPLICAR = r'\*'
t_DIVISION = r'/'
t_IGUAL = r'='
t_IGUALDAD = r'=='
# t_DISTINT = r'!'
t_MENOSQUE = r'<'
t_MAYORQUE = r'>'
t_PUNTOYCOMA = ';'
t_COMA = r','
t_PARENTESISIZ = r'\('
t_PARENTESISDE = r'\)'
t_CORCHETEIZ = r'\['
t_CORCHETEDE = r'\]'
t_LBLOCK = r'{'
t_RBLOCK = r'}'
t_DOBLEPUNTO = r':'
t_HASHTAG = r'\#'
t_PUNTO = r'\.'
t_COMILLASIMPLE = r'\''
t_COMILLASDOBLES = r'\"'
t_QUESTIONMARK = r'\?'


def t_IGUALDADES(t):
    r'([a-zA-Z]([\w])*) ([<|<=|=|>|>=])\s*[a-zA-Z]([\w])*'
    return t


def t_CONECTARLISTA1(t):
    r'((\[\d*\w*\])?|(\w+)?)+ \s* (\++) ((\[\d*\w*\]?)|(\w+)?)+ \s* (\++) ((\[\d*\w*\])?|(\w+)?)+'
    return t


def t_CONECTARLISTA2(t):
    r'([a-zA-Z]+\s*(\++)[a-zA-Z]+)'
    return t


def t_CONS(t):
    r'([\d*])?([a-zA-Z]([\w])*)?\:([\d*])?([a-zA-Z]([\w])*)?'
    return t


def t_CADENAVACIA(t):
    r'\[]'
    return t


def t_NUMBER(t):
    r'\d+'
    t.value = float(t.value)
    return t


def t_VARIABLE(t):
    r'[a-zA-Z]([\w])*'
    if t.value in reserved:
        t.type = reserved[t.value]  # Check for reserved words
        return t
    else:
        return t


""" def t_VARIABLE2(t):
    r'[a-zA-Z](\w)*'
    if t.value in reserved:
        t.type = reserved[t.value]  # Check for reserved words
        return t
    else:
        return t """

def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    if t.value in reserved:
        t.type = reserved[t.value]  # Check for reserved words
        return t
    else:
        t_error(t)

def t_CADENA1(t):
    r'(\"[a-z A-Z]*)\s*([a-z A-Z]*\")'
    return t

def t_MENOSOIGUAL(t):
    r'<='
    return t

def t_MAYOROIGUAL(t):
    r'>='
    return t

def t_ASSIGN(t):
    r'=>'
    return t

def t_DEQUAL(t):
    r'!='
    return t

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

def t_space(t):
    r'\s+'
    t.lexer.lineno += len(t.value)

t_ignore = ' \t'

def t_COMENTARIOENLINEAS(t):
    r'((\{)(\-)\w*\s*\n\w*\-\})'
    t.lexer.lineno += t.value.count('\n')

def t_COMENTARIOSENLINEA(t):
    r'((\-\-)([a-zA-Z,\d*,\s*]*)*\n)'
    t.lexer.lineno += 1

def t_error(t):
    print("Lexical error: " + str(t.value))
    t.lexer.skip(1)

def test(data, lexer):
    lexer.input(data)
    i = 1
    while True:
        tok = lexer.token()
        if not tok:
            break

        print("\t" + str(i) + " - " + "Line: " + str(tok.lineno) + "\t" + str(tok.type) + "\t-->  " + str(tok.value))
        i += 1

def test(data, lexer):
    lexer.input(data)
    i = 1
    print("|--------------------------------------------------------------------------------|") 
    print("|\t" + "ID" + "\t|\t" + "Linea" + "\t|\t" + "Componente lexico" + "\t|\t" + "Lexema" + "\t|")
    while True:
        tok = lexer.token()
        print("|--------------------------------------------------------------------------------|")
        if not tok:
            break
        print("|\t" + str(i) + "\t|\t" + str(tok.lineno) + "\t|\t" + str(tok.type) + "\t|\t" + str(tok.value)+ "\t|")
        i += 1

lexer = lex.lex()

if __name__ == '__main__':
    if len(sys.argv) <= 1:
        fin = 'haskell_codigo.hs'
    else:
        fin = sys.argv[1]
    f = open(fin, 'r')
    data = f.read()
    test(data, lexer)
