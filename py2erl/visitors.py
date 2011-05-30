# -*- coding: utf-8 -*-

import logging
from compiler.visitor import ASTVisitor, walk
import erl
from errors import NotSupportedNode

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
        raise NotSupportedNode(node.__class__)
        return ASTVisitor.default(self, node)

    def visitFunction(self, node):
        print 'aaaa', node.__class__
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

    def visitStmt(self, node):
        for n in node.getChildren():
            walk(n, self, self)

class FunctionVisitor(ASTVisitor):

    def __init__(self):
        ASTVisitor.__init__(self)
        self.children = []

    def default(self, node, *args):
        raise NotSupportedNode(node.__class__)
        return ASTVisitor.default(self, node)

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

    def visitStmt(self, node):
        for n in node.getChildren():
            walk(n, self, self)

    def visitReturn(self, node):
        (value,) = node.getChildren()
        walk(value, self, self)
