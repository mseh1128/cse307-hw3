import ply.yacc as yacc
import ply.lex as lex
import re
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
    def __init__(self, actualType, value):
        super().__init__()
        self.type = "number"
        self.actualType = actualType
        self.value = value

    def eval(self):
        return self.value

    def typecheck(self):
        return self.type

    def __str__(self):
        res = "\t" * self.parentCount() + "Number: " + str(self.value)
        return res


class String(Node):
    def __init__(self, value):
        super().__init__()
        self.type = "string"
        self.value = value

    def eval(self):
        return self.value

    def typecheck(self):
        return self.type

    def __str__(self):
        res = "\t" * self.parentCount() + "String: " + self.value
        return res


class Addition(Node):
    def __init__(self, left, right):
        super().__init__()
        self.left = left
        self.right = right
        self.left.parent = self
        self.right.parent = self

    def typecheck(self):
        leftType = self.left.typecheck()
        rightType = self.right.typecheck()
        print("LEFT TYPE IS " + str(leftType))
        print("RIGHT TYPE IS " + str(rightType))
        if(leftType in ["number", "string", "list"] and leftType == rightType):
            return leftType
        return False

    def eval(self):
        if(self.typecheck()):
            return self.left.eval()+self.right.eval()
        print("Semantic Error")

    def __str__(self):
        res = "\t" * self.parentCount() + "Addition"
        res += "\n" + str(self.left)
        res += "\n" + str(self.right)
        return res


class Membership(Node):
    def __init__(self, left, right):
        super().__init__()
        self.left = left
        self.right = right
        self.left.parent = self
        self.right.parent = self

    def typecheck(self):
        leftType = self.left.typecheck()
        rightType = self.right.typecheck()
        print("LEFT TYPE IS " + str(leftType))
        print("RIGHT TYPE IS " + str(rightType))
        # anything could be in left
        if(rightType in ["string", "list"]):
            return 'Boolean'
        return False

    def eval(self):
        if(self.typecheck()):
            return self.left.eval() in self.right.eval()
        print("Semantic Error")

    def __str__(self):
        res = "\t" * self.parentCount() + "Membership"
        res += "\n" + str(self.left)
        res += "\n" + str(self.right)
        return res


class Cons(Node):
    def __init__(self, left, right):
        super().__init__()
        self.left = left
        self.right = right
        self.left.parent = self
        self.right.parent = self

    def typecheck(self):
        leftType = self.left.typecheck()
        rightType = self.right.typecheck()
        print("LEFT TYPE IS " + str(leftType))
        print("RIGHT TYPE IS " + str(rightType))
        # anything could be in leftType
        if(rightType in ["list"]):
            return True
        return False

    def eval(self):
        if(self.typecheck()):
            self.right.value.insert(0, self.left)
            return self.right.eval()
        print("Semantic Error")

    def __str__(self):
        res = "\t" * self.parentCount() + "Cons"
        res += "\n" + str(self.left)
        res += "\n" + str(self.right)
        return res


class Subtraction(Node):
    def __init__(self, left, right):
        super().__init__()
        self.left = left
        self.right = right
        self.left.parent = self
        self.right.parent = self

    def typecheck(self):
        leftType = self.left.typecheck()
        rightType = self.right.typecheck()
        print("LEFT TYPE IS " + str(leftType))
        print("RIGHT TYPE IS " + str(rightType))
        if(leftType in ["number"] and leftType == rightType):
            return leftType
        return False

    def eval(self):
        if(self.typecheck()):
            return self.left.eval()-self.right.eval()
        print("Semantic Error")

    def __str__(self):
        res = "\t" * self.parentCount() + "Subtraction"
        res += "\n" + str(self.left)
        res += "\n" + str(self.right)
        return res


class Times(Node):
    def __init__(self, left, right):
        super().__init__()
        self.left = left
        self.right = right
        self.left.parent = self
        self.right.parent = self

    def typecheck(self):
        leftType = self.left.typecheck()
        rightType = self.right.typecheck()
        if(leftType in ["number"] and leftType == rightType):
            return leftType
        return False

    def eval(self):
        if(self.typecheck()):
            return self.left.eval()*self.right.eval()
        print("Semantic Error")

    def __str__(self):
        res = "\t" * self.parentCount() + "Multiplication"
        res += "\n" + str(self.left)
        res += "\n" + str(self.right)
        return res

        # 2nd


class Divide(Node):
    def __init__(self, left, right):
        super().__init__()
        self.left = left
        self.right = right
        self.left.parent = self
        self.right.parent = self

    def typecheck(self):
        leftType = self.left.typecheck()
        rightType = self.right.typecheck()
        if(leftType in ["number"] and leftType == rightType and self.right.eval() != 0):
            return leftType
        return False

    def eval(self):
        if(self.typecheck()):
            return float(self.left.eval()/self.right.eval())
        print("Semantic Error")

    def __str__(self):
        res = "\t" * self.parentCount() + "Divide"
        res += "\n" + str(self.left)
        res += "\n" + str(self.right)
        return res


class IntDivide(Node):
    def __init__(self, left, right):
        super().__init__()
        self.left = left
        self.right = right
        self.left.parent = self
        self.right.parent = self

    def typecheck(self):
        leftType = self.left.typecheck()
        rightType = self.right.typecheck()
        if(leftType in ["number"] and leftType == rightType and self.right.eval() != 0):
            return leftType
        return False

    def eval(self):
        if(self.typecheck()):
            return self.left.eval()//self.right.eval()
        print("Semantic Error")

    def __str__(self):
        res = "\t" * self.parentCount() + "IntDivide"
        res += "\n" + str(self.left)
        res += "\n" + str(self.right)
        return res


class Modulus(Node):
    def __init__(self, left, right):
        super().__init__()
        self.left = left
        self.right = right
        self.left.parent = self
        self.right.parent = self

    def typecheck(self):
        leftType = self.left.typecheck()
        rightType = self.right.typecheck()
        if(leftType in ["number"] and self.left.actualType == "integer" and self.left.actualType == self.right.actualType):
            return self.left.actualType
        return False

    def eval(self):
        if(self.typecheck()):
            return self.left.eval() % self.right.eval()
        print("Semantic Error")

    def __str__(self):
        res = "\t" * self.parentCount() + "Modulus"
        res += "\n" + str(self.left)
        res += "\n" + str(self.right)
        return res

# 3rd


class AST_True(Node):
    def __init__(self):
        super().__init__()
        self.value = True
        self.type = "Boolean"

    def typecheck(self):
        return self.type

    def eval(self):
        return self.value

    def __str__(self):
        res = "\t" * self.parentCount() + "True"
        return res


class AST_False(Node):
    def __init__(self):
        super().__init__()
        self.value = False
        self.type = "Boolean"

    def typecheck(self):
        return self.type

    def eval(self):
        return self.value

    def __str__(self):
        res = "\t" * self.parentCount() + "False"
        return res


class Conjunction(Node):
    def __init__(self, left, right):
        super().__init__()
        self.left = left
        self.right = right
        self.left.parent = self
        self.right.parent = self

    def typecheck(self):
        leftType = self.left.typecheck()
        rightType = self.right.typecheck()
        print("CONJ: LEFT TYPE IS " + str(leftType))
        print("CONJ: RIGHT TYPE IS " + str(rightType))
        if(leftType == "Boolean" and leftType == rightType):
            return leftType
        return False

    def eval(self):
        if(self.typecheck()):
            return self.left.eval() and self.right.eval()
        print("Semantic Error")

    def __str__(self):
        res = "\t" * self.parentCount() + "Conjunction"
        res += "\n" + str(self.left)
        res += "\n" + str(self.right)
        return res


class Disjunction(Node):
    def __init__(self, left, right):
        super().__init__()
        self.left = left
        self.right = right
        self.left.parent = self
        self.right.parent = self

    def typecheck(self):
        leftType = self.left.typecheck()
        rightType = self.right.typecheck()
        print("LEFT TYPE IS " + str(leftType))
        print("RIGHT TYPE IS " + str(rightType))
        if(leftType == "Boolean" and leftType == rightType):
            return leftType
        return False

    def eval(self):
        if(self.typecheck()):
            return self.left.eval() or self.right.eval()
        print("Semantic Error")

    def __str__(self):
        res = "\t" * self.parentCount() + "Disjunction"
        res += "\n" + str(self.left)
        res += "\n" + str(self.right)
        return res


class Negation(Node):
    def __init__(self, child):
        super().__init__()
        self.child = child
        self.child.parent = self

    def typecheck(self):
        childType = self.child.typecheck()
        print("CHILD TYPE IS " + str(childType))
        if(childType == "Boolean"):
            return childType
        return False

    def eval(self):
        if(self.typecheck()):
            return not self.child.eval()

    def __str__(self):
        res = "\t" * self.parentCount() + "Negation"
        res += "\n" + str(self.child)
        return res


class LessThan(Node):
    def __init__(self, left, right):
        super().__init__()
        self.left = left
        self.right = right
        self.left.parent = self
        self.right.parent = self

    def typecheck(self):
        leftType = self.left.typecheck()
        rightType = self.right.typecheck()
        if(leftType in ["string", "number"] and leftType == rightType):
            return 'Boolean'  # result will be a boolean
        return False

    def eval(self):
        if(self.typecheck()):
            return self.left.eval() < self.right.eval()
        print("Semantic Error")

    def __str__(self):
        res = "\t" * self.parentCount() + "Less Than"
        res += "\n" + str(self.left)
        res += "\n" + str(self.right)
        return res


class LessThanEqualTo(Node):
    def __init__(self, left, right):
        super().__init__()
        self.left = left
        self.right = right
        self.left.parent = self
        self.right.parent = self

    def typecheck(self):
        leftType = self.left.typecheck()
        rightType = self.right.typecheck()
        if(leftType in ["string", "number"] and leftType == rightType):
            return 'Boolean'  # result will be a boolean
        return False

    def eval(self):
        if(self.typecheck()):
            return self.left.eval() <= self.right.eval()
        print("Semantic Error")

    def __str__(self):
        res = "\t" * self.parentCount() + "Less Than Equal To"
        res += "\n" + str(self.left)
        res += "\n" + str(self.right)
        return res


class EqualTo(Node):
    def __init__(self, left, right):
        super().__init__()
        self.left = left
        self.right = right
        self.left.parent = self
        self.right.parent = self

    def typecheck(self):
        leftType = self.left.typecheck()
        rightType = self.right.typecheck()
        if(leftType in ["string", "number"] and leftType == rightType):
            return 'Boolean'  # result will be a boolean
        return False

    def eval(self):
        if(self.typecheck()):
            return self.left.eval() == self.right.eval()
        print("Semantic Error")

    def __str__(self):
        res = "\t" * self.parentCount() + "Equal To"
        res += "\n" + str(self.left)
        res += "\n" + str(self.right)
        return res


class NotEqualTo(Node):
    def __init__(self, left, right):
        super().__init__()
        self.left = left
        self.right = right
        self.left.parent = self
        self.right.parent = self

    def typecheck(self):
        leftType = self.left.typecheck()
        rightType = self.right.typecheck()
        if(leftType in ["string", "number"] and leftType == rightType):
            return 'Boolean'  # result will be a boolean
        return False

    def eval(self):
        if(self.typecheck()):
            return self.left.eval() != self.right.eval()
        print("Semantic Error")

    def __str__(self):
        res = "\t" * self.parentCount() + "Not Equal To"
        res += "\n" + str(self.left)
        res += "\n" + str(self.right)
        return res


class GreaterThanEqualTo(Node):
    def __init__(self, left, right):
        super().__init__()
        self.left = left
        self.right = right
        self.left.parent = self
        self.right.parent = self

    def typecheck(self):
        leftType = self.left.typecheck()
        rightType = self.right.typecheck()
        if(leftType in ["string", "number"] and leftType == rightType):
            return 'Boolean'  # result will be a boolean
        return False

    def eval(self):
        if(self.typecheck()):
            return self.left.eval() >= self.right.eval()
        print("Semantic Error")

    def __str__(self):
        res = "\t" * self.parentCount() + "Greater Than Equal To"
        res += "\n" + str(self.left)
        res += "\n" + str(self.right)
        return res


class GreaterThan(Node):
    def __init__(self, left, right):
        super().__init__()
        self.left = left
        self.right = right
        self.left.parent = self
        self.right.parent = self

    def typecheck(self):
        leftType = self.left.typecheck()
        rightType = self.right.typecheck()
        if(leftType in ["string", "number"] and leftType == rightType):
            return 'Boolean'  # result will be a boolean
        return False

    def eval(self):
        if(self.typecheck()):
            return self.left.eval() > self.right.eval()
        print("Semantic Error")

    def __str__(self):
        res = "\t" * self.parentCount() + "Greater Than"
        res += "\n" + str(self.left)
        res += "\n" + str(self.right)
        return res


class TupleIndexing(Node):
    def __init__(self, child, idx):
        super().__init__()
        self.type = "TupleIndexing"
        self.child = child
        self.idx = idx
        self.child.parent = self
        self.idx.parent = self

    def typecheck(self):
        childType = self.child.typecheck()
        idxType = self.idx.typecheck()
        if(childType in ["tuple"] and (idxType == "number" and self.idx.actualType == "integer")):
            nodeAtIdx = self.child.getNodeAtIndex(self.idx.eval())
            if(nodeAtIdx == None):
                print("Semantic Error")
                return None
            else:
                return nodeAtIdx.typecheck()
            return False

    def eval(self):
        if(self.typecheck()):
            nodeAtIdx = (self.child.getNodeAtIndex(self.idx.eval()))
            if(nodeAtIdx == None):
                print("Semantic Error")
                return None
            else:
                return nodeAtIdx.eval()
        print("Semantic Error")

    def __str__(self):
        res = "\t" * self.parentCount() + "TupleIndexing"
        res += "\n" + str(self.child)
        res += "\n" + str(self.idx)
        return res


class RegularIndexing(Node):
    def __init__(self, child, idx):
        super().__init__()
        self.type = "RegularIndexing"
        self.child = child
        self.idx = idx
        self.child.parent = self
        self.idx.parent = self

    def typecheck(self):
        childType = self.child.typecheck()
        idxType = self.idx.typecheck()
        if(childType in ["list", "string"] and (idxType == "number" and self.idx.actualType == "integer")):
            if(childType == "list"):
                nodeAtIdx = (self.child.getNodeAtIndex(self.idx.eval()))
                if(nodeAtIdx == None):
                    print("Semantic Error")
                    return None
                else:
                    return nodeAtIdx.typecheck()
            else:
                # if string make sure whatever at index is not null!
                idx_val = self.idx.eval()
                if(idx_val < 0 or idx_val >= len(self.child.eval())):
                    print("Invalid index")
                    print("Semantic Error")
                    return None
                else:
                    return childType
        return False

    def eval(self):
        if(self.typecheck()):
            return self.child.eval()[self.idx.eval()]
            # nodeAtIdx = (self.child.getNodeAtIndex(self.idx.eval()))
            # if(nodeAtIdx == None):
            #     print("Semantic Error")
            #     return None
            # else:
            #     return nodeAtIdx.eval()
        print("Semantic Error")

    def __str__(self):
        res = "\t" * self.parentCount() + "ListOrStringIndexing"
        res += "\n" + str(self.child)
        res += "\n" + str(self.idx)
        return res


class List(Node):
    def __init__(self, initialNode=None):
        super().__init__()
        self.type = "list"
        if initialNode is None:
            self.value = []
        else:
            # list's value is its node
            self.value = [initialNode]
            initialNode.parent = self
            print("INITIAL NODE IS: ")
            print(initialNode)

    def typecheck(self):
        return self.type

    def addNodeToList(self, nodeToAdd):
        nodeToAdd.parent = self
        self.value.insert(0, nodeToAdd)

    def getNodeAtIndex(self, idx):
        if(idx < 0 or idx >= len(self.value)):
            print("Invalid index")
            return None
        else:
            return self.value[idx]

    def eval(self):
        evalNodes = []
        for node in self.value:
            evalNodes.append(node.eval())
        return evalNodes

    def __str__(self):
        res = "\t" * self.parentCount() + "List"
        for childNode in self.value:
            res += "\t" * self.parentCount() + "\n" + str(childNode)
        return res


class Tuple(Node):
    def __init__(self, initialNode=None):
        super().__init__()
        self.type = "tuple"
        if initialNode is None:
            self.value = []
        else:
            # list's value is its node
            self.value = [initialNode]
            initialNode.parent = self
            print("INITIAL NODE IS: ")
            print(initialNode)

    def typecheck(self):
        return self.type

    def addNodeToTuple(self, nodeToAdd):
        nodeToAdd.parent = self
        self.value.insert(0, nodeToAdd)

    def getNodeAtIndex(self, idx):
        idx = idx-1
        if(idx < 0 or idx >= len(self.value)):
            print("Invalid index")
            return None
        else:
            return self.value[idx]

    def eval(self):
        evalNodes = []
        for node in self.value:
            evalNodes.append(node.eval())
        return tuple(evalNodes)

    def __str__(self):
        res = "\t" * self.parentCount() + "Tuple: "
        for childNode in self.value:
            res += "\t" * self.parentCount() + "\n" + str(childNode)
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


def t_STRING(t):
    r'(\'([^\'\"]*)\')|(\"([^\'\"]*)\")'
    try:
        # p = re.compile('(\'([^\'\"]*)\')|(\"([^\'\"]*)\")')
        # regexMatch = p.match(t.value)
        if(lexer.lexmatch.group(3)):
            t.value = lexer.lexmatch.group(3)
        else:
            t.value = lexer.lexmatch.group(5)
    except Exception as e:
        print("Error occurred %s", e)
    return t


def t_REAL(t):
    r'((\d+\.\d*)|(\d*\.\d+))(e-?\d+)?'
    try:
        t.value = float(t.value)
    except ValueError:
        print("Float value too large %d", t.value)
        t.value = 0
    return t


def t_INTEGER(t):
    r'\d+'
    # uminus to account for subtraction!
    print(t.value)
    try:
        t.value = int(t.value)
    except ValueError:
        print("Integer value too large %d", t.value)
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
              ('left', 'INDEXING'),
              ('left', 'TUPLE_INDEXING'),
              ('left', 'TUPLE_CREATION'),
              ('left', 'PARENTHETICAL_EXPR'),
              )


def p_expr(p):
    '''expr : prop'''
    p[0] = p[1]


def p_prop_plus(p):
    '''prop : prop PLUS prop'''
    p[0] = Addition(p[1], p[3])


def p_prop_minus(p):
    'prop : prop MINUS prop'
    p[0] = Subtraction(p[1], p[3])


def p_prop_times(p):
    '''prop : prop TIMES prop'''
    p[0] = Times(p[1], p[3])


def p_prop_divide(p):
    'prop : prop DIVIDE prop'
    p[0] = Divide(p[1], p[3])


def p_prop_intDivide(p):
    'prop : prop INT_DIVIDE prop'
    p[0] = IntDivide(p[1], p[3])


def p_prop_modulus(p):
    'prop : prop MOD prop'
    p[0] = Modulus(p[1], p[3])


def p_prop_membership(p):
    'prop : prop IN prop'
    p[0] = Membership(p[1], p[3])


def p_prop_uminus(p):
    'prop : MINUS INTEGER %prec UMINUS'
    # only integers can be -, not floats
    p[0] = Number('integer', p[2]*-1)


def p_prop_lessThan(p):
    'prop : prop LESS_THAN prop'
    p[0] = LessThan(p[1], p[3])


def p_prop_lessThanEqualTo(p):
    'prop : prop LESS_THAN_EQUAL_TO prop'
    p[0] = LessThanEqualTo(p[1], p[3])


def p_prop_equalTo(p):
    'prop : prop EQUAL_TO prop'
    p[0] = EqualTo(p[1], p[3])


def p_prop_notEqualTo(p):
    'prop : prop NOT_EQUAL_TO prop'
    p[0] = NotEqualTo(p[1], p[3])


def p_prop_greaterThanEqualTo(p):
    'prop : prop GREATER_THAN_EQUAL_TO prop'
    p[0] = GreaterThanEqualTo(p[1], p[3])


def p_prop_greaterThan(p):
    'prop : prop GREATER_THAN prop'
    p[0] = GreaterThan(p[1], p[3])


def p_prop_cons(p):
    'prop : prop CONS prop'
    # only integers can be -, not floats
    p[0] = Cons(p[1], p[3])


def p_prop_negation(p):
    'prop : NEGATION prop'
    p[0] = Negation(p[2])


def p_prop_conjunction(p):
    'prop : prop CONJUNCTION prop'
    p[0] = Conjunction(p[1], p[3])


def p_prop_disjunction(p):
    'prop : prop DISJUNCTION prop'
    p[0] = Disjunction(p[1], p[3])


def p_prop_parenthetical(p):
    'prop : LEFT_PARENTHESIS prop RIGHT_PARENTHESIS %prec PARENTHETICAL_EXPR'
    p[0] = p[2]


def p_prop_true(p):
    'prop : TRUE'
    p[0] = AST_True()


def p_prop_false(p):
    'prop : FALSE'
    p[0] = AST_False()


def p_prop_integer(p):
    'prop : INTEGER'
    p[0] = Number('integer', p[1])


def p_prop_real(p):
    'prop : REAL'
    p[0] = Number('real', p[1])


def p_prop_string(p):
    'prop : STRING'
    p[0] = String(p[1])


def p_prop_list(p):
    '''prop : LEFT_BRACKET prop_contents RIGHT_BRACKET
              | LEFT_BRACKET RIGHT_BRACKET
    '''
    if(len(p)-1 == 2):
        p[0] = List()
    else:
        # p[0] = List(p[2])
        p[0] = p[2]


def p_prop_list_contents(p):
    '''prop_contents : prop
    | prop COMMA prop_contents
    '''
    if(len(p)-1 == 1):
        print("Starting list")
        p[0] = List(p[1])
    else:
        print("Adding to existing list")
        p[3].addNodeToList(p[1])
        p[0] = p[3]


def p_prop_tuple(p):
    ''' prop : LEFT_PARENTHESIS prop COMMA prop_tup_contents RIGHT_PARENTHESIS %prec TUPLE_CREATION
            | LEFT_PARENTHESIS prop COMMA RIGHT_PARENTHESIS %prec TUPLE_CREATION'''
    if(len(p)-1 == 4):
        # only 1 element in tuple
        p[0] = Tuple(p[2])
    else:
        p[4].addNodeToTuple(p[2])
        p[0] = p[4]


def p_prop_tuple_contents(p):
    '''prop_tup_contents : prop
                        | prop COMMA prop_tup_contents
    '''
    if(len(p)-1 == 1):
        print("Starting Tuple")
        p[0] = Tuple(p[1])
    else:
        print("Adding to existing Tuple")
        p[3].addNodeToTuple(p[1])
        p[0] = p[3]


def p_prop_tup_indexing(p):
    'prop : HASHTAG prop prop %prec TUPLE_INDEXING'
    p[0] = TupleIndexing(p[3], p[2])


def p_prop_ListString_indexing(p):
    ''' 
        prop : prop LEFT_BRACKET prop RIGHT_BRACKET %prec INDEXING 
    '''
    p[0] = RegularIndexing(p[1], p[3])


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
        print(result)
        if result is not None:
            print("Evaluation:", result.eval())


if __name__ == "__main__":
    main()
