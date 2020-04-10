import ply.lex as lex
tokens = (
    'INTEGER',
    'REAL',
    'TRUE',
    'FALSE',
    'STRING',
    'LEFT_PARENTHESIS',
    'RIGHT_PARENTHESIS',
    'COMMA',
    'HASHTAG',
    'LEFT_BRACKET',
    'RIGHT_BRACKET',
    'RAISED_TO_POWER_OF',
    'TIMES',
    'DIVIDE',
    'INT_DIVIDE',
    'MOD',
    'PLUS',
    'MINUS',
    'IN',
    'CONS',
    'NEGATION',
    'CONJUNCTION',
    'DISJUNCTION',
    'LESS_THAN',
    'LESS_THAN_EQUAL_TO',
    'EQUAL_TO',
    'NOT_EQUAL_TO',
    'GREATER_THAN_EQUAL_TO',
    'GREATER_THAN',
)

# maybe exclude -0 as an option

t_TRUE = r'True'
t_FALSE = r'False'
t_STRING = r'(\'[^\'\"]*\')|(\"[^\'\"]*\")'
t_LEFT_PARENTHESIS = r'\('
t_RIGHT_PARENTHESIS = r'\)'
t_COMMA = r','
t_HASHTAG = r'\#'
t_LEFT_BRACKET = r'\['
t_RIGHT_BRACKET = r']'
t_RAISED_TO_POWER_OF = r'\*{2}'
t_TIMES = r'\*'
t_DIVIDE = r'/'
t_INT_DIVIDE = r'div'
t_MOD = r'mod'
t_PLUS = r'\+'
t_MINUS = r'-'
t_IN = r'in'
t_CONS = r'::'
t_NEGATION = r'not'
t_CONJUNCTION = r'andalso'
t_DISJUNCTION = r'orelse'
t_LESS_THAN = r'<'
t_LESS_THAN_EQUAL_TO = r'<='
t_EQUAL_TO = r'=='
t_NOT_EQUAL_TO = r'<>'
t_GREATER_THAN_EQUAL_TO = r'>='
t_GREATER_THAN = r'>'

# Function based definition of VARIABLE tokens.


def t_INTEGER(t):
    r'-?\d+'
    try:
        t.value = int(t.value)
    except ValueError:
        print("Integer value too large %d", t.value)
        t.value = 0
    return t


def t_REAL(t):
    r'((\d+\.\d*)|(\d*\.\d+))(e-?\d+)?'
    try:
        t.value = float(t.value)
    except ValueError:
        print("Float value too large %d", t.value)
        t.value = 0
    return t

# def t_VARIABLE(t):
#     r'[a-z]\d*'
#     return t


t_ignore = ' \t'


def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")


def t_error(t):
    print("Illegal Character '%s', at %d, %d" %
          (t.value[0], t.lineno, t.lexpos))
    t.lexer.skip(1)


# Build lexer
lexer = lex.lex(debug=True)

# This function only calls the lexer to tokenize input. You should try something
# like this when you begin writing your grammar to make sure your source inputs
# are being broken up into tokens properly.


def tokenize(inp):
    lexer.input(inp)
    while True:
        tok = lexer.token()
        if not tok:
            break
        print(tok)


tokenize('\'hello\'')
