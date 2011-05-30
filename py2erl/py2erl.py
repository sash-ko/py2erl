# -*- coding: utf-8 -*-

"""Primitive and naive Python to Erlang compiler.

Main principle: convert Python abstract syntax tree (AST) to
Erlang abstract form and compile it to binary code.

Inspired by:
http://stackoverflow.com/questions/1974236/string-to-abstract-syntax-tree
"""

import sys
import os
import compiler
import subprocess
from compiler.visitor import walk
from visitors import ModuleVisitor
import logging
from errors import AbstractFormError
import erl

logging.basicConfig(level=logging.DEBUG,
                    format='%(module)s:%(funcName)s %(asctime)s - %(message)s')

def module_name(file_name):
    return os.path.basename(os.path.splitext(file_name)[0])

COMPILER = 'compile_forms'

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
                forms = erl.full_af(erl.module_af(module_name(fname)),
                                    visitor.exports, visitor.functions)

                retcode = subprocess.call(['erlc',
                    'py2erl/erl/{compiler}.erl'.format(compiler=COMPILER)])
                if not retcode:
                    retcode = subprocess.call(['erl', '-noshell', '-eval',
                        "code:load_file('{compiler}'),"\
                        "{compiler}:compile({forms}),"\
                        "erlang:halt()".format(forms=forms, compiler=COMPILER)])
                    if not retcode:
                        logging.info('Successfully compiled')
                    else:
                        logging.error('Can not compile abstract forms')
                else:
                    logging.error('Can not compile compiler')
        except AbstractFormError, e:
            logging.error('Abstract form error: {0}', e)
            logging.exception(e)
        except IOError, e:
            logging.error('Error while reading {0}', fname)
            logging.exception(e)
        except SyntaxError, e:
            logging.error('Invalid input file syntax')
            logging.exception(e)
        except Exception, e:
            logging.error('Error occured {0}', e)
            logging.exception(e)
    else:
        logging.info('Usage: python py2erl.py python_in')
