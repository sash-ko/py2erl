# -*- coding: utf-8 -*-

import logging
from errors import AbstractFormError

def variable_af(varname, lineno=1):
    """Variable abstract form (variable name).
    Require further values binding.
    """

    varname = varname.capitalize()
    fmt = locals()
    return "{{var, {lineno}, '{varname}'}}".format(**fmt)

def number_af(val, lineno=1):
    fmt = locals()
    if isinstance(val, float):
        fmt['typename'] = 'float'
    elif isinstance(val, int):
        fmt['typename'] = 'integer'
    else:
        raise AbstractFormError('Number', val)
    return '{{{typename}, {lineno}, {val}}}'.format(**fmt)

def addition_af(left, right, lineno=1):
    """Abstract form of addition operation"""
    return _binary_op_af('+', left, right, lineno)

def substraction_af(left, right, lineno=1):
    """Abstract form of addition operation"""
    return _binary_op_af('-', left, right, lineno)

def _binary_op_af(op, left, right, lineno=1):
    """Abstract form of binary operations"""

    fmt = locals()
    return "{{op, {lineno}, '{op}', {left}, {right}}}".format(**fmt)

def _clause_af(arguments, body, lineno=1):
    arguments = ','.join([variable_af(arg) for arg in arguments])
    fmt = locals()
    return '{{clause, {lineno}, [{arguments}], [], [{body}]}}'.format(**fmt)

def function_af(fn_name, argnames, clauses, lineno=1):
    logging.warning('Only one function clause supported')
    clauses = ','.join([_clause_af(argnames, c, lineno) for c in clauses])
    fmt = {'fn_name': fn_name, 'argnum': len(argnames), 'clauses': clauses,
            'lineno': lineno}
    return '{{function, {lineno}, {fn_name}, {argnum}, [{clauses}]}}'.format(**fmt)

def stmt_form(nodes):
    logging.warning('There is no abstract equivalent of Pythons Stmt')
    return ','.join(nodes)

def return_form(nodes):
    logging.warning('There is no abstract equivalent of Pythons Return')
    return ','.join(nodes)
