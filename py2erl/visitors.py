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

    def default(self, node, *args):
        logging.warning('Skip node %s', node)
        return ASTVisitor.default(self, node)

    def visitFunction(self, node):
        # (decorators, name, argnames, defaults, flags, docs, code)
        cn = node.getChildren()
        fn_name = cn[1]
        argnames = cn[2]
        code = cn[-1]
        visitor = FunctionVisitor()
        walk(code, visitor, visitor)
        self.functions.append(erl.function_af(fn_name, argnames,
                                                visitor.children))
        self.exports.append(erl.export_af(fn_name, argnames))

class FunctionVisitor(ASTVisitor):

    def __init__(self):
        ASTVisitor.__init__(self)
        self.children = []

    def _visitOp(self, node, fn):
        (left, right) = node.getChildren()
        visitor = FunctionVisitor()
        walk(left, visitor, visitor)
        walk(right, visitor, visitor)
        self.children.append(fn(*visitor.children))

    def visitAdd(self, node):
        self._visitOp(node, erl.addition_af)

    def visitSub(self, node):
        self._visitOp(node, erl.substraction_af)

    def visitMul(self, node):
        self._visitOp(node, erl.multiplication_af)

    def visitDiv(self, node):
        self._visitOp(node, erl.division_af)

    def visitMod(self, node):
        """ '%'opertation. Depends on context - can be format or
        mod operator"""
        logging.warning('Not implemented. Depends on context')

    def visitPrintnl(self, node):
        # node children format: ([node1, node2, ...,] dist)
        nodes = node.getChildren()
        nodes = nodes[0]
        visitor = FunctionVisitor()
        for n in nodes:
            walk(nodes, visitor, visitor)

    def visitName(self, node):
        (name, ) = node.getChildren()
        self.children.append(erl.variable_af(name))

    def visitConst(self, node):
        (value, ) = node.getChildren()
        self.children.append(erl.term_af(value))

    #def visitReturn(self, node):
    #    (value,) = node.getChildren()
    #    visitor = ModuleVisitor()
    #    walk(value, visitor)
    #    self.tree.append(erl.return_form(visitor.tree))

