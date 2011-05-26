# -*- coding: utf-8 -*-

#import logging
from compiler.visitor import ASTVisitor, walk
import erl

class ModuleVisitor(ASTVisitor):
    """Recursively traverse Python AST and convert
    it to Erlang abstract form
    """

    def __init__(self):
        ASTVisitor.__init__(self)
        self.tree = []

    def visitStmt(self, node):
        visitor = ModuleVisitor()
        for c in node.nodes:
            walk(c, visitor)
        self.tree.append(erl.stmt_form(visitor.tree))

    def visitFunction(self, node):
        # (decorators, name, argnames, defaults, flags, docs, code)
        cn = node.getChildren()
        visitor = ModuleVisitor()
        walk(cn[-1], visitor)
        self.tree.append(erl.function_form(cn[1], cn[2], visitor.tree))

    def visitReturn(self, node):
        (value,) = node.getChildren()
        visitor = ModuleVisitor()
        walk(value, visitor)
        self.tree.append(erl.return_form(visitor.tree))

    def visitAdd(self, node):
        (left, right) = node.getChildren()
        visitor = ModuleVisitor()
        walk(left, visitor)
        walk(right, visitor)
        self.tree.append(erl.add_form(*visitor.tree))

    def visitName(self, node):
        (name, ) = node.getChildren()
        self.tree.append(erl.name_form(name))

    def visitConst(self, node):
        (value, ) = node.getChildren()
        self.tree.append(erl.const_form(value))
