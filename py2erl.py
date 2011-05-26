# -*- coding: utf-8 -*-

"""Primitive and naive Python to Erlang compiler.

Main principle: convert Python abstract syntax tree (AST) to Erlang AST
and compile it to binary code.

Inspired by:
http://stackoverflow.com/questions/1974236/string-to-abstract-syntax-tree
"""

import sys
import compiler
from compiler.visitor import walk
from visitors import ModuleVisitor
import logging

logging.basicConfig(level=logging.DEBUG,
                    format='%(module)s:%(funcName)s %(asctime)s - %(message)s')

if __name__ == '__main__':

    if len(sys.argv) == 2:
        fname = sys.argv[1]
        erl_out = None
        try:
            module_tree = compiler.parseFile(fname)
            if isinstance(module_tree, compiler.ast.Module):
                doc, module_body = module_tree.getChildren()

                visitor = ModuleVisitor()
                walk(module_tree, visitor)
                for c in visitor.tree:
                    print c

            #erl_out = compose_ast(visitor.erl_module,
            #                        visitor.erl_exports,
            #                        visitor.erl_functions)
        except IOError, e:
            print 'Error while reading', fname
            logging.exception(e)
        except SyntaxError, e:
            print 'Invalid input file syntax'
            logging.exception(e)
        except Exception, e:
            print 'Error occured', e
            logging.exception(e)

        #if erl_out:
        #    with open('gen.erl', 'w') as out:
        #        out.write(erl_out)
        #        print 'Output saved to gen.erl'
    else:
        print 'Usage: python py2erl.py python_in'
