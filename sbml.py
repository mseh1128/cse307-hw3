import ply.yacc as yacc
import ply.lex as lex
import re
import sys

# NAME: Manav Sehgal
# ID#: 111581702


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

    def getActualType(self):
        return self.actualType

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
        self.type = "Addition"
        self.left.parent = self
        self.right.parent = self

    def typecheck(self):
        leftType = self.left.typecheck()
        rightType = self.right.typecheck()
        # print("LEFT TYPE IS " + str(leftType))
        # print("RIGHT TYPE IS " + str(rightType))
        if(leftType in ["number", "string", "list"] and leftType == rightType):
            return leftType
        return False

    def eval(self):
        if(self.typecheck()):
            a = self.left
            b = self.right
            if(self.right.type not in ["number", "string", "list"]):
                # print("about to evalulate again")
                b = self.right.eval()
            if(self.left.type not in ["number", "string", "list"]):
                # print("about to evalulate again")
                a = self.left.eval()
            if(a.typecheck() == "number"):
                if(a.getActualType() == "integer" and b.getActualType() == "integer"):
                    return Number("integer", a.eval() + b.eval())
                else:
                    return Number("real", a.eval() + b.eval())
            elif(a.typecheck() == "string"):
                return String(a.eval() + b.eval())
            else:
                # must be list
                a.value.extend(b.value)
                return a
        else:
            print("SEMANTIC ERROR")
            sys.exit()

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
        self.type = "Membership"
        self.left.parent = self
        self.right.parent = self

    def typecheck(self):
        leftType = self.left.typecheck()
        rightType = self.right.typecheck()
        # print("LEFT TYPE IS " + str(leftType))
        # print("RIGHT TYPE IS " + str(rightType))
        # anything could be in left
        if(rightType in ["string", "list"]):
            return 'Boolean'
        return False

    def eval(self):
        if(self.typecheck()):
            a = self.right
            b = self.left
            if(self.right.type not in ["string", "list"]):
                # print("about to evalulate again")
                a = self.right.eval()
            if(self.left.type == 'variable'):
                b = self.left.eval()
            if(b.eval() in a.eval()):
                return AST_True()
            else:
                return AST_False()
        else:
            print("SEMANTIC ERROR")
            sys.exit()
        # if(self.typecheck()):
        #     return self.left.eval() in self.right.eval()

        # print("SEMANTIC ERROR")

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
        self.type = "Cons"
        self.left.parent = self
        self.right.parent = self

    def typecheck(self):
        leftType = self.left.typecheck()
        rightType = self.right.typecheck()
        # print("LEFT TYPE IS " + str(leftType))
        # print("RIGHT TYPE IS " + str(rightType))
        # anything could be in leftType
        if(rightType in ["list"]):
            return 'list'
        return False

    def eval(self):
        if(self.typecheck()):
            a = self.right
            b = self.left
            if(self.right.type not in ["list"]):
                # print("about to evalulate again")
                a = self.right.eval()
            if(self.left.type == "variable"):
                b = self.left.eval()
            a.value.insert(0, b)
            return self.right
        else:
            print("SEMANTIC ERROR")
            sys.exit()

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
        self.type = "Subtraction"
        self.left.parent = self
        self.right.parent = self

    def typecheck(self):
        leftType = self.left.typecheck()
        rightType = self.right.typecheck()
        # print("LEFT TYPE IS " + str(leftType))
        # print("RIGHT TYPE IS " + str(rightType))
        if(leftType in ["number"] and leftType == rightType):
            return leftType
        return False

    def eval(self):
        if(self.typecheck()):
            a = self.left
            b = self.right
            if(self.right.type not in ["number"]):
                # print("about to evalulate again")
                b = self.right.eval()
            if(self.left.type not in ["number"]):
                # print("about to evalulate again")
                a = self.left.eval()
            if(a.getActualType() == "integer" and b.getActualType() == "integer"):
                return Number("integer", a.eval() - b.eval())
            else:
                return Number("real", a.eval() - b.eval())
        else:
            print("SEMANTIC ERROR")
            sys.exit()
        # if(self.typecheck()):
        #     return self.left.eval()-self.right.eval()

        # print("SEMANTIC ERROR")

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
        self.type = "Times"
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
            a = self.left
            b = self.right
            if(self.right.type not in ["number"]):
                # print("about to evalulate again")
                b = self.right.eval()
            if(self.left.type not in ["number"]):
                # print("about to evalulate again")
                a = self.left.eval()
            if(a.getActualType() == "integer" and b.getActualType() == "integer"):
                return Number("integer", a.eval() * b.eval())
            else:
                return Number("real", a.eval() * b.eval())
        else:
            print("SEMANTIC ERROR")
            sys.exit()

    def __str__(self):
        res = "\t" * self.parentCount() + "Multiplication"
        res += "\n" + str(self.left)
        res += "\n" + str(self.right)
        return res

        # 2nd


class Exponentation(Node):
    def __init__(self, left, right):
        super().__init__()
        self.left = left
        self.right = right
        self.type = "Exponentiation"
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
            a = self.left
            b = self.right
            if(self.right.type not in ["number"]):
                # print("about to evalulate again")
                b = self.right.eval()
            if(self.left.type not in ["number"]):
                # print("about to evalulate again")
                a = self.left.eval()
            if(a.getActualType() == "integer" and b.getActualType() == "integer"):
                return Number("integer", a.eval() ** b.eval())
            else:
                return Number("real", a.eval() ** b.eval())
        else:
            print("SEMANTIC ERROR")
            sys.exit()

    def __str__(self):
        res = "\t" * self.parentCount() + "Exponentiation"
        res += "\n" + str(self.left)
        res += "\n" + str(self.right)
        return res

        # 2nd


class Divide(Node):
    def __init__(self, left, right):
        super().__init__()
        self.left = left
        self.right = right
        self.type = "Divide"
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
            a = self.left
            b = self.right
            if(self.right.type not in ["number"]):
                # print("about to evalulate again")
                b = self.right.eval()
            if(self.left.type not in ["number"]):
                # print("about to evalulate again")
                a = self.left.eval()
            return Number("real", float(a.eval()/b.eval()))
        else:
            print("SEMANTIC ERROR")
            sys.exit()
        # if(self.typecheck()):
        #     return float(self.left.eval()/self.right.eval())

        # print("SEMANTIC ERROR")

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
        self.type = "IntDivide"
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
            a = self.left
            b = self.right
            if(self.right.type not in ["number"]):
                # print("about to evalulate again")
                b = self.right.eval()
            if(self.left.type not in ["number"]):
                # print("about to evalulate again")
                a = self.left.eval()
            return Number("integer", a.eval()//b.eval())
        else:
            print("SEMANTIC ERROR")
            sys.exit()
        # if(self.typecheck()):
        #     return self.left.eval()//self.right.eval()

        # print("SEMANTIC ERROR")

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
        self.type = "Modulus"
        self.left.parent = self
        self.right.parent = self

    def typecheck(self):
        leftType = self.left.typecheck()
        rightType = self.right.typecheck()
        if((leftType in ["number"] and self.left.getActualType() == "integer")
            and (rightType in ["number"] and self.left.getActualType() == "integer") and (self.left.getActualType() == self.right.getActualType())
           ):
            return leftType
        return False

    def eval(self):
        if(self.typecheck()):
            a = self.left
            b = self.right
            if(self.right.type not in ["number"]):
                # print("about to evalulate again")
                b = self.right.eval()
            if(self.left.type not in ["number"]):
                # print("about to evalulate again")
                a = self.left.eval()
            return Number("integer", a.eval() % b.eval())
        else:
            print("SEMANTIC ERROR")
            sys.exit()
        # if(self.typecheck()):
        #     return self.left.eval() % self.right.eval()

        # print("SEMANTIC ERROR")

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
        self.type = "Conjunction"
        self.left.parent = self
        self.right.parent = self

    def typecheck(self):
        leftType = self.left.typecheck()
        rightType = self.right.typecheck()
        # print("CONJ: LEFT TYPE IS " + str(leftType))
        # print("CONJ: RIGHT TYPE IS " + str(rightType))
        if(leftType == "Boolean" and leftType == rightType):
            return leftType
        return False

    def eval(self):
        if(self.typecheck()):
            a = self.left
            b = self.right
            if(self.right.type not in ["Boolean"]):
                # print("about to evalulate again")
                b = self.right.eval()
            if(self.left.type not in ["Boolean"]):
                a = self.left.eval()
            if(a.eval() and b.eval()):
                return AST_True()
            else:
                return AST_False()
        else:
            print("SEMANTIC ERROR")
            sys.exit()
        # if(self.typecheck()):
        #     return self.left.eval() and self.right.eval()

        # print("SEMANTIC ERROR")

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
        self.type = "Disjunction"
        self.left.parent = self
        self.right.parent = self

    def typecheck(self):
        leftType = self.left.typecheck()
        rightType = self.right.typecheck()
        # print("LEFT TYPE IS " + str(leftType))
        # print("RIGHT TYPE IS " + str(rightType))
        if(leftType == "Boolean" and leftType == rightType):
            return leftType
        return False

    def eval(self):
        if(self.typecheck()):
            a = self.left
            b = self.right
            if(self.right.type not in ["Boolean"]):
                # print("about to evalulate again")
                b = self.right.eval()
            if(self.left.type not in ["Boolean"]):
                # print("about to evalulate again")
                a = self.left.eval()
            if(a.eval() or b.eval()):
                return AST_True()
            else:
                return AST_False()
        else:
            print("SEMANTIC ERROR")
            sys.exit()
        # if(self.typecheck()):
        #     return self.left.eval() or self.right.eval()

        # print("SEMANTIC ERROR")

    def __str__(self):
        res = "\t" * self.parentCount() + "Disjunction"
        res += "\n" + str(self.left)
        res += "\n" + str(self.right)
        return res


class Negation(Node):
    def __init__(self, child):
        super().__init__()
        self.child = child
        self.type = "Negation"
        self.child.parent = self

    def typecheck(self):
        childType = self.child.typecheck()
        # print("CHILD TYPE IS " + str(childType))
        if(childType == "Boolean"):
            return childType
        return False

    def eval(self):
        if(self.typecheck()):
            a = self.child
            if(self.child.type not in ["Boolean"]):
                # print("about to evalulate again")
                a = self.child.eval()
            if(not a.eval()):
                return AST_True()
            else:
                return AST_False()
        else:
            print("SEMANTIC ERROR")
            sys.exit()

    def __str__(self):
        res = "\t" * self.parentCount() + "Negation"
        res += "\n" + str(self.child)
        return res


class LessThan(Node):
    def __init__(self, left, right):
        super().__init__()
        self.left = left
        self.right = right
        self.type = "LessThan"
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
            a = self.left
            b = self.right
            if(self.right.type not in ["string", "number"]):
                # print("about to evalulate again")
                b = self.right.eval()
            if(self.left.type not in ["string", "number"]):
                # print("about to evalulate again")
                a = self.left.eval()
            if(a.eval() < b.eval()):
                return AST_True()
            else:
                return AST_False()
        else:
            print("SEMANTIC ERROR")
            sys.exit()
        # if(self.typecheck()):
        #     return self.left.eval() < self.right.eval()

        # print("SEMANTIC ERROR")

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
        self.type = "LessThanEqualTo"
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
            a = self.left
            b = self.right
            if(self.right.type not in ["string", "number"]):
                # print("about to evalulate again")
                b = self.right.eval()
            if(self.left.type not in ["string", "number"]):
                # print("about to evalulate again")
                a = self.left.eval()
            if(a.eval() <= b.eval()):
                return AST_True()
            else:
                return AST_False()
        else:
            print("SEMANTIC ERROR")
            sys.exit()
        # if(self.typecheck()):
        #     return self.left.eval() <= self.right.eval()

        # print("SEMANTIC ERROR")

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
        self.type = "EqualTo"
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
            a = self.left
            b = self.right
            if(self.right.type not in ["string", "number"]):
                # print("about to evalulate again")
                b = self.right.eval()
            if(self.left.type not in ["string", "number"]):
                # print("about to evalulate again")
                a = self.left.eval()
            if(a.eval() == b.eval()):
                return AST_True()
            else:
                return AST_False()
        else:
            print("SEMANTIC ERROR")
            sys.exit()
        # if(self.typecheck()):
        #     return self.left.eval() == self.right.eval()

        # print("SEMANTIC ERROR")

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
        self.type = "NotEqualTo"
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
            a = self.left
            b = self.right
            if(self.right.type not in ["string", "number"]):
                # print("about to evalulate again")
                b = self.right.eval()
            if(self.left.type not in ["string", "number"]):
                # print("about to evalulate again")
                a = self.left.eval()
            if(a.eval() != b.eval()):
                return AST_True()
            else:
                return AST_False()
        else:
            print("SEMANTIC ERROR")
            sys.exit()
        # if(self.typecheck()):
        #     return self.left.eval() != self.right.eval()

        # print("SEMANTIC ERROR")

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
        self.type = "GreaterThanEqualTo"
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
            a = self.left
            b = self.right
            if(self.right.type not in ["string", "number"]):
                # print("about to evalulate again")
                b = self.right.eval()
            if(self.left.type not in ["string", "number"]):
                # print("about to evalulate again")
                a = self.left.eval()
            if(a.eval() >= b.eval()):
                return AST_True()
            else:
                return AST_False()
        else:
            print("SEMANTIC ERROR")
            sys.exit()
        # if(self.typecheck()):
        #     return self.left.eval() >= self.right.eval()

        # print("SEMANTIC ERROR")

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
        self.type = "GreaterThan"
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
            a = self.left
            b = self.right
            if(self.right.type not in ["string", "number"]):
                # print("about to evalulate again")
                b = self.right.eval()
            if(self.left.type not in ["string", "number"]):
                # print("about to evalulate again")
                a = self.left.eval()
            if(a.eval() > b.eval()):
                return AST_True()
            else:
                return AST_False()
        else:
            print("SEMANTIC ERROR")
            sys.exit()
        # if(self.typecheck()):
        #     return self.left.eval() > self.right.eval()

        # print("SEMANTIC ERROR")

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
        if(childType in ["tuple"] and (idxType == "number" and self.idx.getActualType() == "integer")):
            nodeAtIdx = self.child.getNodeAtIndex(self.idx.eval())
            if(nodeAtIdx == None):
                print("SEMANTIC ERROR")
                sys.exit()
            else:
                return nodeAtIdx.typecheck()
            return False

    def eval(self):
        if(self.typecheck()):
            a = self.child
            if(self.child.type not in ["tuple"]):
                # print("about to evalulate again")
                a = self.child.eval()
            nodeAtIdx = (a.getNodeAtIndex(self.idx.eval()))
            if(nodeAtIdx == None):
                print("SEMANTIC ERROR")
                sys.exit()
            else:
                return nodeAtIdx
        else:
            print("SEMANTIC ERROR")
            sys.exit()
        # if(self.typecheck()):
        #     nodeAtIdx = (self.child.getNodeAtIndex(self.idx.eval()))
        #     if(nodeAtIdx == None):

        #         print("SEMANTIC ERROR")
        #     else:
        #         return nodeAtIdx.eval()

    def __str__(self):
        res = "\t" * self.parentCount() + "TupleIndexing"
        res += "\n" + str(self.child)
        res += "\n" + str(self.idx)
        return res


class RegularIndexing(Node):
    def __init__(self, child, idxList):
        super().__init__()
        self.type = "RegularIndexing"
        self.child = child
        self.idxList = idxList
        self.child.parent = self
        for node in self.idxList:
            node.parent = self

    def typecheck(self):
        # print("CHILD TYPE IS: " + childType)
        # idxListType = self.idx.typecheck()
        if(self.child.typecheck() in ["list", "string"]):
            for node in self.idxList:
                if(not(node.typecheck() == "number" and node.getActualType() == "integer")):
                    return False
            # print("Valid Type: RegularIndexing")
            return "list"
        else:
            # print("Semantic Error: RegularIndexing")
            return False
        # if(childType in ["list", "string"] and (idxType == "number" and self.idx.getActualType() == "integer")):
            # if(childType == "list"):
            #     if(not isinstance(self.child, RegularIndexing)):
            #         nodeAtIdx = (self.child.getNodeAtIndex(self.idx.eval()))
            #         if(nodeAtIdx == None):

            #             print("SEMANTIC ERROR")
            # sys.exit()
            #         else:
            #             return nodeAtIdx.typecheck()
            #     return 'list'
            # else:
            #     # if string make sure whatever at index is not null!
            #     idx_val = self.idx.eval()
            #     if(idx_val < 0 or idx_val >= len(self.child.eval())):
            #         # print("Invalid index")

            #         print("SEMANTIC ERROR")
            # sys.exit()
            #     else:
            #         return childType
        return False

    def eval(self):
        if(self.typecheck()):
            a = self.child
            if(self.child.type not in ["list", "string"]):
                # print("about to evalulate again")
                a = self.child.eval()
            currA = None
            for node in self.idxList:
                tNode = node
                if(node.type == 'variable'):
                    tNode = node.eval()
                if(currA == None):
                    currA = a.getNodeAtIndex(tNode.eval())
                else:
                    currA = currA.getNodeAtIndex(tNode.eval())
                if(currA == None):
                    print("SEMANTIC ERROR")
                    sys.exit()
            return currA
        else:
            print("SEMANTIC ERROR")
            sys.exit()

    def __str__(self):
        res = "\t" * self.parentCount() + "ListOrStringIndexing"
        res += "\n" + str(self.child)
        res += "\n" + str(self.idxList)
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
            # print("INITIAL NODE IS: ")
            # print(initialNode)

    def typecheck(self):
        return self.type

    def addNodeToList(self, nodeToAdd):
        nodeToAdd.parent = self
        self.value.insert(0, nodeToAdd)

    def updateListNode(self, idx, node):
        self.value[idx.eval()] = node

    def getNodeAtIndex(self, idx):
        if(idx < 0 or idx >= len(self.value)):
            # print("Invalid index")

            print("SEMANTIC ERROR")
            sys.exit()
        else:
            return self.value[idx]

    def eval(self):
        evalNodes = []
        # print('hereee')
        for node in self.value:
            tNode = node
            if(node.type == 'variable'):
                tNode = node.eval()
            evalNodes.append(tNode.eval())
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
            # print("INITIAL NODE IS: ")
            # print(initialNode)

    def typecheck(self):
        return self.type

    def addNodeToTuple(self, nodeToAdd):
        nodeToAdd.parent = self
        self.value.insert(0, nodeToAdd)

    def getNodeAtIndex(self, idx):
        idx = idx-1
        if(idx < 0 or idx >= len(self.value)):
            # print("Invalid index")
            return
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


class Variable(Node):
    def __init__(self, name):
        super().__init__()
        self.name = name
        self.type = "variable"
        if self.name in names:
            names[self.name].parent = self

    # add typecheck?
    def typecheck(self):
        if self.name in names:
            return names[self.name].typecheck()
        else:
            return None

    # add typecheck?
    def getActualType(self):
        # only for numbers
        if self.name in names:
            return names[self.name].getActualType()
        else:
            return None

    def eval(self):
        if self.name in names:
            # print('evaluating variable')
            return names[self.name]
        else:
            print("SEMANTIC ERROR")
            sys.exit()

    def varOnlyEval(self):
        if self.name in names:
            return names[self.name]
        else:
            print("SEMANTIC ERROR")
            sys.exit()

    def __str__(self):
        res = "\t" * self.parentCount() + "Variable: " + self.name + "\n"
        return res


class Assignment(Node):
    def __init__(self, var, propToAssign):
        super().__init__()
        self.var = var
        self.prop = propToAssign
        self.prop.parent = self
        self.type = "assignment"

    # add typecheck?
    def typecheck(self):
        return self.type

    def eval(self):
        # must evaluate to a base type (number, boolean, string, list, or tuple)
        currProp = self.prop
        while(currProp.type not in ["number", "string", "Boolean", "list", "tuple"]):
            # print('current prop is not a valid instance, reducing')
            currProp = self.prop.eval()
        # print('curr prop is now')
        # print(currProp)
        names[self.var] = currProp
        # if(isinstance(self.prop, List)):
        #     print('was instance of list')
        # names[self.var] = self.prop
        # else:
        #     print('was not instance of list')
        #     print(self.prop)
        #     names[self.var] = self.prop.eval()
        #     print(names)

    def __str__(self):
        res = "\t" * self.parentCount() + "Assignment: " + str(self.var)
        res += "\n" + "\t" * self.parentCount() + "To: " + str(self.prop)
        return res


class ListIndexAssignment(Node):
    def __init__(self, var, idx, val):
        super().__init__()
        self.var = var
        self.idx = idx
        self.val = val
        self.type = "ListIndexAssignment"
        self.var.parent = self
        self.idx.parent = self
        self.val.parent = self

    # add typecheck?
    def typecheck(self):
        # val could be anything bc its a list
        # right now allows all lists
        # right now only does not support expressions inside self
        if(self.var.typecheck() == "list" and self.idx.typecheck() == "number"
           and self.idx.getActualType() == "integer"):
            # print("Valid Type: ListIndexAssignment")
            return self.type
        else:
            return False

    def eval(self):
        if(self.typecheck()):
            varList = self.var
            idxVal = self.idx
            varVal = self.val
            while(varList.type not in ["list"]):
                # print('current prop is not a valid instance, reducing')
                varList = varList.eval()
            while(idxVal.type not in ["number"]):
                # print('current prop is not a valid instance, reducing')
                idxVal = idxVal.eval()
            while(varVal.type in ["variable"]):
                # print('current prop is not a valid instance, reducing')
                varVal = varVal.eval()
            # print('hereb')
            varList.updateListNode(idxVal, varVal)
            return None
        else:
            print("SEMANTIC ERROR")
            sys.exit()

    def __str__(self):
        res = "\t" * self.parentCount() + "ListIndexAssignment"
        res += "\n" + str(self.var)
        res += "\n" + str(self.idx)
        res += "\n" + str(self.val)
        return res


class Print(Node):
    def __init__(self, child):
        super().__init__()
        self.child = child
        self.type = "Print"
        self.child.parent = self

    # add typecheck?
    def typecheck(self):
        # can print anything except null?
        # if(self.c)
        return self.type

    def eval(self):
        # print('IN THE PRINT EVAL')
        if(self.typecheck()):
            currChild = self.child
            while(currChild.type not in ["number", "string", "Boolean", "list", "tuple"]):
                # print('current prop is not a valid instance, reducing')
                currChild = self.child.eval()
            # print('printing now')
            # print('here')
            print('here')
            print(currChild.eval())
            return None
        else:
            print("SEMANTIC ERROR")
            sys.exit()

    def __str__(self):
        res = "\t" * self.parentCount() + "Print"
        res += "\n" + str(self.child)
        return res


class Block(Node):
    def __init__(self):
        super().__init__()
        self.type = "block"
        self.value = []

    def typecheck(self):
        return self.type

    def addNodeToList(self, nodeToAdd):
        nodeToAdd.parent = self
        self.value.insert(0, nodeToAdd)

    def eval(self):
        for node in self.value:
            node.eval()

        # evalNodes = []
        # for node in self.value:
        #     evalNodes.append(node.eval())
        # return evalNodes

    def __str__(self):
        res = "\t" * self.parentCount() + "Block"
        for childNode in self.value:
            res += "\t" * self.parentCount() + "\n" + str(childNode)
        return res


class IfStmt(Node):
    def __init__(self, conditional, block):
        super().__init__()
        self.type = "if"
        self.conditional = conditional
        self.block = block
        self.conditional.parent = self
        self.block.parent = self

    def typecheck(self):
        # check if conditional if actually a conditional
        return self.type

    def eval(self):
        if(self.typecheck()):
            a = self.conditional
            if(self.conditional.type not in ["Boolean"]):
                # print("about to evalulate again")
                # print(type(a))
                a = self.conditional.eval()
            # print(type(a))
            if(a.eval()):
                # print('conditon was true in if; now evalulating trueBlock')
                self.block.eval()
            else:
                pass
                # print('conditon was false in if; not evalulating block!')
        else:
            print("SEMANTIC ERROR")
            sys.exit()

    def __str__(self):
        res = "\t" * self.parentCount() + "If"
        res += "\n" + str(self.conditional)
        res += "\n" + str(self.block)
        return res


class IfElseStmt(Node):
    def __init__(self, conditional, trueBlock, falseBlock):
        super().__init__()
        self.type = "ifelse"
        self.conditional = conditional
        self.trueBlock = trueBlock
        self.falseBlock = falseBlock
        self.conditional.parent = self
        self.trueBlock.parent = self
        self.falseBlock.parent = self

    def typecheck(self):
        # check if conditional if actually a conditional
        return self.type

    def eval(self):
        if(self.typecheck()):
            a = self.conditional
            if(self.conditional.type not in ["Boolean"]):
                # print("about to evalulate again")
                a = self.conditional.eval()
            if(a.eval()):
                # print('conditon was true in if; now evalulating trueBlock')
                self.trueBlock.eval()
            else:
                # print('conditon was false in if; not evalulating falseBlock!')
                self.falseBlock.eval()
        else:
            print("SEMANTIC ERROR")
            sys.exit()
        # print("in if else stmt")
        # print(self.conditional)
        # if(self.conditional.eval().eval()):
        #     print('conditon was true in if; now evalulating trueBlock')
        #     self.trueBlock.eval()
        # else:
        #     print('conditon was false in if; not evalulating falseBlock!')
        #     self.falseBlock.eval()

    def __str__(self):
        res = "\t" * self.parentCount() + "Ifelse"
        res += "\n" + str(self.conditional)
        res += "\n" + "\t" * (self.parentCount()+1) + "True Block"
        res += "\n" + str(self.trueBlock)
        res += "\n" + "\t" * (self.parentCount()+1) + "False Block"
        res += "\n" + str(self.falseBlock)
        return res


class WhileLoop(Node):
    def __init__(self, loopCondition, block):
        super().__init__()
        self.type = "while"
        self.loopCondition = loopCondition
        self.block = block
        self.loopCondition.parent = self
        self.block.parent = self

    def typecheck(self):
        # check if conditional if actually a conditional
        self.type

    def eval(self):
        if(self.loopCondition.type == 'Boolean'):
            t = self.loopCondition
            while(t.eval()):
                self.block.eval()
        else:
            t = self.loopCondition.eval()
            while(t.eval()):
                self.block.eval()
                t = self.loopCondition.eval()

        # t = self.loopCondition.eval()
        # no break/continue specified so good enough for now

    def __str__(self):
        res = "\t" * self.parentCount() + "While"
        res += "\n" + str(self.loopCondition)
        res += "\n" + str(self.block)
        return res


class FunctionDef(Node):
    def __init__(self, funcName, formalParams, block, expr):
        super().__init__()
        print("in function def with")
        print("funcName")
        print(funcName)
        print(type(funcName))
        print("formalParams")
        print(formalParams)
        print(type(expr))
        print("block")
        print(block)
        print(type(block))
        print("expr")
        print(expr)
        print(type(expr))
        self.type = "functionDef"
        self.funcName = funcName
        self.formalParams = formalParams
        self.block = block
        self.expr = expr
        self.block.parent = self
        self.expr.parent = self

    def typecheck(self):
        # check if conditional if actually a conditional
        self.type

    def eval(self):
        pass
        # if(self.loopCondition.type == 'Boolean'):
        #     t = self.loopCondition
        #     while(t.eval()):
        #         self.block.eval()
        # else:
        #     t = self.loopCondition.eval()
        #     while(t.eval()):
        #         self.block.eval()
        #         t = self.loopCondition.eval()

        # t = self.loopCondition.eval()
        # no break/continue specified so good enough for now

    def __str__(self):
        res = "\t" * self.parentCount() + "Function Definition: " + str(self.funcName)
        res += "\n" + str(self.formalParams)
        res += "\n" + str(self.block)
        return res


class FunctionCall(Node):
    def __init__(self, funcName, args):
        super().__init__()
        print("in function call")
        self.type = "functionCall"
        self.funcName = funcName
        self.args = args
        self.funcName.parent = self

    def typecheck(self):
        # check if conditional if actually a conditional
        self.type

    def eval(self):
        if(self.funcName in functionNames):
            print('found this function name!')
            print(s)
        else:
            print('COULD NOT FIND THIS FUNC NAME!')
            print("SEMANTIC ERROR")
            sys.exit()

        # if(self.loopCondition.type == 'Boolean'):
        #     t = self.loopCondition
        #     while(t.eval()):
        #         self.block.eval()
        # else:
        #     t = self.loopCondition.eval()
        #     while(t.eval()):
        #         self.block.eval()
        #         t = self.loopCondition.eval()

        # t = self.loopCondition.eval()
        # no break/continue specified so good enough for now

    def __str__(self):
        res = "\t" * self.parentCount() + "Function Call: " + str(self.funcName)
        res += "\n" + str(self.args)
        return res


reserved = {
    'True': 'TRUE',
    'False': 'FALSE',
    'div': 'INT_DIVIDE',
    'mod': 'MOD',
    'in': 'IN',
    'not': 'NEGATION',
    'andalso': 'CONJUNCTION',
    'orelse': 'DISJUNCTION',
    'print': 'PRINT',
    'if': 'IF',
    'else': 'ELSE',
    'while': 'WHILE',
    'fun': 'FUN'
}


tokens = [
    'INTEGER',
    'REAL',
    'STRING',
    'LEFT_PARENTHESIS',
    'RIGHT_PARENTHESIS',
    'COMMA',
    'HASHTAG',
    'LEFT_BRACKET',
    'RIGHT_BRACKET',
    'LEFT_CURLY_BRACE',
    'RIGHT_CURLY_BRACE',
    'RAISED_TO_POWER_OF',
    'TIMES',
    'DIVIDE',
    'PLUS',
    'MINUS',
    'CONS',
    'LESS_THAN',
    'LESS_THAN_EQUAL_TO',
    'EQUAL_TO',
    'NOT_EQUAL_TO',
    'GREATER_THAN_EQUAL_TO',
    'GREATER_THAN',
    'VARIABLE',
    'EQUALS',
    'SEMICOLON'
] + list(reserved.values())

# maybe exclude -0 as an option

t_EQUALS = r'='
t_LEFT_PARENTHESIS = r'\('
t_RIGHT_PARENTHESIS = r'\)'
t_COMMA = r','
t_SEMICOLON = r';'
t_HASHTAG = r'\#'
t_LEFT_BRACKET = r'\['
t_RIGHT_BRACKET = r']'
t_LEFT_CURLY_BRACE = r'{'
t_RIGHT_CURLY_BRACE = r'}'
t_RAISED_TO_POWER_OF = r'\*{2}'
t_TIMES = r'\*'
t_DIVIDE = r'/'
t_PLUS = r'\+'
t_MINUS = r'-'
t_CONS = r'::'
t_LESS_THAN = r'<'
t_LESS_THAN_EQUAL_TO = r'<='
t_EQUAL_TO = r'=='
t_NOT_EQUAL_TO = r'<>'
t_GREATER_THAN_EQUAL_TO = r'>='
t_GREATER_THAN = r'>'

# Function based definition of VARIABLE tokens.


def t_VARIABLE(t):
    r'[a-zA-Z][a-zA-Z0-9_]*'
    t.type = reserved.get(t.value, 'VARIABLE')
    return t


def t_STRING(t):
    r'(\'([^\'\"]*)\')|(\"([^\'\"]*)\")'
    try:
        # p = re.compile('(\'([^\'\"]*)\')|(\"([^\'\"]*)\")')
        # regexMatch = p.match(t.value)
        if(lexer.lexmatch.group(4)):
            t.value = lexer.lexmatch.group(4)
        else:
            t.value = lexer.lexmatch.group(6)
    except Exception as e:
        pass
        # print("Error occurred %s", e)
    return t


def t_REAL(t):
    r'((\d+\.\d*)|(\d*\.\d+))(e-?\d+)?'
    try:
        t.value = float(t.value)
    except ValueError:
        # print("Float value too large %d", t.value)
        t.value = 0
    return t


def t_INTEGER(t):
    r'\d+'
    # uminus to account for subtraction!
    # print(t.value)
    try:
        t.value = int(t.value)
    except ValueError:
        # print("Integer value too large %d", t.value)
        t.value = 0
    return t


t_ignore = ' \t'


def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")


def t_error(t):
    # print("Illegal Character '%s', at %d, %d" %
    #       (t.value[0], t.lineno, t.lexpos))
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


names = {}
functionNames = {}

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
               'MOD'),
              ('right', 'UMINUS'),
              ('right', 'RAISED_TO_POWER_OF'),
              ('left', 'TUPLE_INDEXING'),
              ('left', 'TUPLE_CREATION'),
              ('left', 'PARENTHETICAL_EXPR'),
              )


def p_outer_block(p):
    '''outer_block : funcDefMult block'''
    p[0] = p[2]


def p_func_defMult(p):
    '''
        funcDefMult : 
                    | funcDef funcDefMult

    '''
    pass


def p_func_def(p):
    '''
        funcDef : FUN VARIABLE LEFT_PARENTHESIS varList RIGHT_PARENTHESIS EQUALS block prop SEMICOLON
                | FUN VARIABLE LEFT_PARENTHESIS RIGHT_PARENTHESIS EQUALS block prop SEMICOLON
    '''
    varList = []
    funcName = p[2]
    block = None
    expr = None  # actually expression
    if(len(p)-1 == 9):
        print('varlist exists')
        varList = p[4]
        block = p[7]
        expr = p[8]
    else:
        print('varlist does not exist!')
        block = p[6]
        expr = p[7]

    p[0] = FunctionDef(funcName, varList, block, expr)
    functionNames[funcName] = p[0]


def p_var_list(p):
    '''
        varList : VARIABLE
                | VARIABLE COMMA varList
    '''
    print('in var list')
    print(len(p))
    if(len(p)-1 == 1):
        # print('length greater than one')
        p[0] = [p[1]]
    else:
        # print('length not greater than 1')
        p[3].insert(0, p[1])
        p[0] = p[3]
    print(p[0])


def p_func_call(p):
    '''
        prop : prop_BS LEFT_PARENTHESIS propList RIGHT_PARENTHESIS
             | prop_BS LEFT_PARENTHESIS RIGHT_PARENTHESIS
    '''
    argList = []
    funcName = p[1]
    if(len(p)-1 == 4):
        argList = p[3]
    p[0] = FunctionCall(funcName, argList)


def p_prop_propList(p):
    '''
        propList : prop
                 | prop COMMA propList
    '''
    print('in prop list')
    print(len(p))
    if(len(p)-1 == 1):
        p[0] = [p[1]]
    else:
        p[3].insert(0, p[1])
        p[0] = p[3]
    print(p[0])


def p_stat_block(p):
    '''block : LEFT_CURLY_BRACE block_contents RIGHT_CURLY_BRACE'''
    p[0] = p[2]


def p_stat_block_contents(p):
    '''block_contents :
                        | stat block_contents
    '''
    if(len(p) > 1):
        # print('length greater than one')
        p[2].addNodeToList(p[1])
        p[0] = p[2]
    else:
        # print('length not greater than 1')
        p[0] = Block()
    # if(isinstance(p[2], Block)):
    pass
    # ie if not empty, block_contents exists


def p_stat_assign(p):
    'stat : VARIABLE EQUALS prop SEMICOLON'
    p[0] = Assignment(p[1], p[3])


def p_stat_ListIndex_assign(p):
    '''
        stat : prop_BS LEFT_BRACKET prop RIGHT_BRACKET EQUALS prop SEMICOLON
    '''
    p[0] = ListIndexAssignment(p[1], p[3], p[6])


def p_stat_Print(p):
    '''
        stat : PRINT LEFT_PARENTHESIS prop RIGHT_PARENTHESIS SEMICOLON
    '''
    p[0] = Print(p[3])


def p_stat_While(p):
    '''
        stat : WHILE LEFT_PARENTHESIS prop RIGHT_PARENTHESIS block
    '''
    p[0] = WhileLoop(p[3], p[5])


def p_stat_IfElse(p):
    '''
        stat : IF LEFT_PARENTHESIS prop RIGHT_PARENTHESIS block ELSE block
    '''
    p[0] = IfElseStmt(p[3], p[5], p[7])


def p_stat_If(p):
    '''
        stat : IF LEFT_PARENTHESIS prop RIGHT_PARENTHESIS block
    '''
    p[0] = IfStmt(p[3], p[5])

# def p_func_call(p):
#     '''
#         stat : IF LEFT_PARENTHESIS prop RIGHT_PARENTHESIS block
#     '''
#     p[0] = IfStmt(p[3], p[5])


def p_prop_plus(p):
    '''prop : prop PLUS prop'''
    p[0] = Addition(p[1], p[3])


def p_prop_minus(p):
    'prop : prop MINUS prop'
    p[0] = Subtraction(p[1], p[3])


def p_prop_times(p):
    '''prop : prop TIMES prop'''
    p[0] = Times(p[1], p[3])


def p_prop_exponentation(p):
    '''prop : prop RAISED_TO_POWER_OF prop'''
    p[0] = Exponentation(p[1], p[3])


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


def p_prop_variable(p):
    'prop_BS : VARIABLE'
    p[0] = Variable(p[1])


def p_prop_true(p):
    'prop : TRUE'
    p[0] = AST_True()


def p_prop_false(p):
    'prop : FALSE'
    p[0] = AST_False()


def p_prop_integer(p):
    'prop : INTEGER'
    p[0] = Number('integer', p[1])


def p_prop_uminus(p):
    'prop : MINUS INTEGER %prec UMINUS'
    # only integers can be -, not floats
    p[0] = Number('integer', p[2]*-1)


def p_prop_real(p):
    'prop : REAL'
    p[0] = Number('real', p[1])


def p_prop_string(p):
    'prop : STRING'
    p[0] = String(p[1])


def p_prop_list(p):
    '''prop_BS : LEFT_BRACKET prop_contents RIGHT_BRACKET
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
        # print("Starting list")
        p[0] = List(p[1])
    else:
        # print("Adding to existing list")
        p[3].addNodeToList(p[1])
        p[0] = p[3]


def p_prop_ListString_indexing(p):
    '''
        prop : prop_BS bracket_prop_content
    '''
    p[0] = RegularIndexing(p[1], p[2])


def p_prop_lsi_content(p):
    '''
        bracket_prop_content : LEFT_BRACKET prop RIGHT_BRACKET
                            | LEFT_BRACKET prop RIGHT_BRACKET bracket_prop_content
    '''
    if(len(p) == 4):
        p[0] = [p[2]]
    else:
        p[4].insert(0, p[2])
        p[0] = p[4]


def p_prop_something(p):
    '''
        prop : prop_BS
    '''
    p[0] = p[1]


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
        # print("Starting Tuple")
        p[0] = Tuple(p[1])
    else:
        # print("Adding to existing Tuple")
        p[3].addNodeToTuple(p[1])
        p[0] = p[3]


def p_prop_tup_indexing(p):
    'prop : HASHTAG prop prop %prec TUPLE_INDEXING'
    p[0] = TupleIndexing(p[3], p[2])


def p_error(p):
    # print("Syntax error at (%d, %d)" % (p.lineno, p.lexpos))
    print("SYNTAX ERROR")
    sys.exit()


parser = yacc.yacc(debug=True)


def parse(inp):
    result = parser.parse(inp, debug=1)
    return result


def main():
    # inp = input("Enter a proposition: ")
    # tokenize(inp)
    # with open(sys.argv[1]) as fp:
    #     result = parse(fp.read())
        # print(result)
        # if result is not None:
        #     result.eval()

    # inp = '''
    # {
    #     number = 33;
    #     test = 40;
    # }

    # '''
    # tokenize(inp)
    # parse(inp)
    # print("Parsing finished")
    tBLOCK = '''
        fun factorial(n, a, c) =
        {
            if(n < 1) {
                output = 1;
            }
            else
            {
                output = n * factorial(n - 1);
            }
        }
        output;

        {
            print('hello');
        }
     '''

    tokenize(tBLOCK)
    result = parse(tBLOCK)
    print(result)
    if result is not None:
        result.eval()


if __name__ == "__main__":
    main()
