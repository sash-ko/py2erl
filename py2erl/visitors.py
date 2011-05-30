# -*- coding: utf-8 -*-

import logging
from compiler.visitor import ASTVisitor, walk
import erl

class ModuleVisitor(ASTVisitor):
    """Recursively traverse Python AST and convert
    it to Erlang abstract form
    """

    def __init__(self):
        # no super, ASTVisitor is old style class
        ASTVisitor.__init__(self)
        self.functions = []
        self.exports = []

    def default(self, node):
        logging.warning('Skip node', node)
        return ASTVisitor.default(self, node)

    def visitFunction(self, node):
        # (decorators, name, argnames, defaults, flags, docs, code)
        cn = node.getChildren()
        fn_name = cn[1]
        argnames = cn[2]
        code = cn[-1]
        visitor = FunctionVisitor()
        walk(code, visitor)
        self.functions.append(erl.function_af(fn_name, argnames,
                                                visitor.children))
        self.exports.append(erl.export_af(fn_name, argnames))

class FunctionVisitor(ASTVisitor):

    def __init__(self):
        ASTVisitor.__init__(self)
        self.children = []

    def visitAdd(self, node):
        (left, right) = node.getChildren()
        visitor = FunctionVisitor()
        walk(left, visitor)
        walk(right, visitor)
        self.children.append(erl.addition_af(*visitor.children))

    def visitSub(self, node):
        (left, right) = node.getChildren()
        visitor = FunctionVisitor()
        walk(left, visitor)
        walk(right, visitor)
        self.children.append(erl.substraction_af(*visitor.children))

    def visitName(self, node):
        (name, ) = node.getChildren()
        self.children.append(erl.variable_af(name))

    def visitConst(self, node):
        (value, ) = node.getChildren()
        self.children.append(erl.number_af(value))

    #def visitReturn(self, node):
    #    (value,) = node.getChildren()
    #    visitor = ModuleVisitor()
    #    walk(value, visitor)
    #    self.tree.append(erl.return_form(visitor.tree))

