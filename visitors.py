# -*- coding: utf-8 -*-

import logging
from compiler.visitor import ASTVisitor
import erl

class NodeVisitor(ASTVisitor):
    """General Python AST walker.

    Collect information about module, exports and functions.

    In order to visit particular node you need to implement function:
    'visitWhatYouNeedName'.
    """

    def __init__(self, mod_name):
        # no super - ASTVisitor is old-style class
        ASTVisitor.__init__(self)
        self.erl_module = erl.create_module(mod_name)
        self.erl_exports = []
        self.erl_functions = []

    def default(self, node):
        logging.debug('Not processed %s', node)
        return ASTVisitor.default(self, node)

    def visitImport(self, node):
        """Collect all imports. Not implemented"""
        (names) = node.getChildren()
        logging.debug('Import visited (%s)', names)

    def visitFrom(self, node, *args):
        """Collect from imports. Not implemented"""
        (mod_name, what, n) = node.getChildren()
        logging.debug('From visited (%s)', mod_name)

    def visitFunction(self, node):
        """Collect functions and exports"""

        # (decorators, name, argnames, defaults, flags, docs, code)
        children = node.getChildren()

        fn_name = children[1]
        argnames = children[2]
        code = children[5]

        self.erl_exports.append(erl.create_export(fn_name, len(argnames)))

        logging.warning('Functions support is not implemented')
        #walk(code, FunctionVisitor(fn))

class FunctionVisitor(ASTVisitor):
    """Reseacher of the function code"""

    def __init__(self, fn_object):
        ASTVisitor.__init__(self)
        self.fn_object = fn_object

    def visitPrintnl(self, node):
        """Collect prints"""

