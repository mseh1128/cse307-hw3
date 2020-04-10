import ply.yacc as yacc
import ply.lex as lex
import sys


class Node():
    def __init__(self):
        self.parent = None

    def parentCount(self):
        count = 0
        current = self.parent
        while current is not None:
            count += 1
            current = current.parent
        return count

 class Number(Node):
     def __init__(self,value):
         super().__init__()

    def eval(self):
        if((self.left == self.right) or (isinstance(self.left, (int, float)) and isinstance(self.right, (int, float)))):
            if(isinstance(self.left, (int, float, str, list))):
                print("IN HERE, RESULT IS: " + str(self.left+self.right))
                return self.left+self.right
            else:
                print("SEMANTIC ERROR")
        else:
            print("SEMANTIC ERROR")

    def __str__(self):
        res = "\t" * self.parentCount() + "Addition"
        res += "\n" + str(self.left)
        res += "\n" + str(self.right)
        return res


class Addition(Node):
    def __init__(self, left, right):
        super().__init__()
        self.left = left
        self.right = right

    def eval(self):
        if((self.left == self.right) or (isinstance(self.left, (int, float)) and isinstance(self.right, (int, float)))):
            if(isinstance(self.left, (int, float, str, list))):
                print("IN HERE, RESULT IS: " + str(self.left+self.right))
                return self.left+self.right
            else:
                print("SEMANTIC ERROR")
        else:
            print("SEMANTIC ERROR")

    def __str__(self):
        res = "\t" * self.parentCount() + "Addition"
        res += "\n" + str(self.left)
        res += "\n" + str(self.right)
        return res


class Subtraction(Node):
    def __init__(self, left, right):
        super().__init__()
        self.left = left
        self.right = right

    def eval(self):
        if((self.left == self.right) or (isinstance(self.left, (int, float)) and isinstance(self.right, (int, float)))):
            if(isinstance(self.left, (int, float, str, list))):
                return self.left-self.right
            else:
                print("SEMANTIC ERROR")
        else:
            print("SEMANTIC ERROR")

    def __str__(self):
        res = "\t" * self.parentCount() + "Subtraction"
        res += "\n" + str(self.left)
        res += "\n" + str(self.right)
        return res


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


precedence = (('left', 'DISJUNCTION'),
              ('left', 'CONJUNCTION'),
              ('left', 'NEGATION'),
              ('left', 'LESS_THAN', 'LESS_THAN_EQUAL_TO', 'EQUAL_TO',
               'NOT_EQUAL_TO',
               'GREATER_THAN_EQUAL_TO',
               'GREATER_THAN',),
              ('right', 'CONS'),
              ('left', 'IN'),
              ('left', 'PLUS', 'MINUS'),
              ('left', 'TIMES',
               'DIVIDE',
               'INT_DIVIDE',
               'MOD', ),
              ('right', 'UMINUS'),
              ('right', 'RAISED_TO_POWER_OF'),
              #   ('left', 'INDEXING'),
              #   ('left', 'TUPLE_INDEXING'),
              #   ('left', 'TUPLE_CREATION'),
              #   ('left', 'PARANTHETICAL_EXPR'),
              )


def p_expr(p):
    'expr : prop'
    p[0] = p[1]


def p_prop_plus(p):
    'prop : prop PLUS prop'
    p[0] = Addition(p[1], p[3])


def p_prop_minus(p):
    'prop : prop MINUS prop'
    # p[0] = Subtraction(p[1], p[3])


def p_prop_uminus(p):
    'prop : MINUS prop %prec UMINUS'
    p[0] = -p[2]


def p_prop_parenthetical(p):
    'prop : LEFT_PARENTHESIS prop RIGHT_PARENTHESIS'
    p[0] = p[2]


# def p_prop_pimes(p):
#     'prop : prop pIMES prop'

# def p_prop_divide(p):
#     'prop : prop DIVIDE prop'

# def p_prop_group(p):
#     'prop : LPAREN prop RPAREN'


def p_prop_integer(p):
    'prop : INTEGER'
    p[0] = p[1]


def p_prop_real(p):
    'prop : REAL'
    p[0] = p[1]


def p_error(p):
    print("Syntax error at '%s' (%d, %d)" % (p.value, p.lineno, p.lexpos))
    sys.exit()


parser = yacc.yacc(debug=True)


def parse(inp):
    result = parser.parse(inp, debug=1)
    return result


def main():
    while True:
        inp = input("Enter a proposition: ")
        # tokenize(inp)
        result = parse(inp)
        print(result.eval())
        # if result is not None:
        #     print("Evaluation:", result.eval())


if __name__ == "__main__":
    main()
