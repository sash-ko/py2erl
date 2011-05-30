# -*- coding: utf-8 -*-

import logging
from compiler.visitor import ASTVisitor, walk
import erl
from errors import NotSupportedNode

class ModuleVisitor(ASTVisitor):
    """Traverse Python Abstract syntax tree on module level.
    Collect information about function declarations, namely
    create export attribute (e.g. '-export([fn_name/arity]).')
    for every function and function body in Erlang abstract fromat.

    AST node handler - function with name "visitNodeName", e.g.
    "visitFunction".

    Raise exception NotSupportedNode if meet can not find aproptiate handler.

    Note: line numbers are not supported.
    """

    def __init__(self):
        # no super, ASTVisitor is old style class
        ASTVisitor.__init__(self)
        self.functions = []
        self.exports = []

    def default(self, node, *args):
        """Default node handler."""
        raise NotSupportedNode(node.__class__)
        return ASTVisitor.default(self, node)

    def visitFunction(self, node):
        """Create export attribute and start traverse
        function body (depth-first search)"""
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
        """Continue traversing in depth"""
        for n in node.getChildren():
            walk(n, self, self)

class FunctionVisitor(ASTVisitor):
    """Traverse function body (depth-first search).

    Raise exception NotSupportedNode if meet can not find aproptiate handler.

    Note: only one function clause is supported, e.g.
        def test(a, b):
            if a == 0:
                return None
            return a * b
    can not be converted to:
        test(0, _) -> 0;
        test(A, B) -> A*B.
    """

    def __init__(self):
        ASTVisitor.__init__(self)
        self.children = []

    def default(self, node, *args):
        raise NotSupportedNode(node.__class__)
        return ASTVisitor.default(self, node)

    def _visitOp(self, node, fn):
        """Traverse binary operators like '+', '-', '/', etc.
        fn - function which abstract representation of particular operation.
        """
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
        """Print to io:format.
        Print has several forms:
            print 'test'
            print 'test', var
            print 'test %s' % var
            print 'test {0}'.format(var)
        """
        # node children format: ([node1, node2, ...,] dist)
        nodes = node.getChildren()
        nodes = nodes[0]
        visitor = FunctionVisitor()
        for n in nodes:
            walk(nodes, visitor, visitor)
        visitor.children.append(erl.nil_af())
        self.children.append(erl.io_format_af(visitor.children))

    def visitName(self, node):
        """Variable name.

        def test(a):
            #...

        visitName('a')
        """
        (name, ) = node.getChildren()
        self.children.append(erl.variable_af(name))

    def visitConst(self, node):
        """Constant value.
        1 + 2

        visitConst(1)
        visitConst(2)
        """
        (value, ) = node.getChildren()
        self.children.append(erl.term_af(value))

    def visitStmt(self, node):
        """Continue traversing in depth"""
        for n in node.getChildren():
            walk(n, self, self)

    def visitReturn(self, node):
        """Continue traversing in depth"""
        (value,) = node.getChildren()
        walk(value, self, self)
