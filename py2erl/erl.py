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

def term_af(val, lineno=1):
    fmt = locals()
    if isinstance(val, float):
        fmt['typename'] = 'float'
    elif isinstance(val, int):
        fmt['typename'] = 'integer'
    elif isinstance(val, basestring):
        fmt['typename'] = 'string'
    else:
        raise AbstractFormError('Number', val)
    return '{{{typename}, {lineno}, {val}}}'.format(**fmt)

def addition_af(left, right, lineno=1):
    """Abstract form of addition operation"""
    return _binary_op_af('+', left, right, lineno)

def substraction_af(left, right, lineno=1):
    """Abstract form of substraction operation"""
    return _binary_op_af('-', left, right, lineno)

def multiplication_af(left, right, lineno=1):
    """Abstract form of multiplication operation"""
    return _binary_op_af('*', left, right, lineno)

def division_af(left, right, lineno=1):
    """Abstract form of division operation"""
    return _binary_op_af('/', left, right, lineno)


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
    fn_templ = '{{function, {lineno}, {fn_name}, {argnum}, [{clauses}]}}'
    return fn_templ.format(**fmt)

def _attribute_af(at_name, attribute, lineno=1):
    fmt = locals()
    return '{{attribute, {lineno}, {at_name}, {attribute}}}'.format(**fmt)

def export_af(fn_name, argnames, lineno=1):
    attribute = '[{{{fn_name}, {arity}}}]'.format(fn_name=fn_name,
                                                    arity=len(argnames))
    return _attribute_af('export', attribute, lineno)

def module_af(name, lineno=1):
    return _attribute_af('module', name, lineno)

def full_af(module, exports, functions):
    fmt = locals()
    fmt['exports'] = ','.join(exports)
    fmt['functions'] = ','.join(functions)
    return '[{module}, {exports}, {functions}]'.format(**fmt)

def io_format_af(items, lineno=1):
    pass
