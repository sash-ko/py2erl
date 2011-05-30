# -*- coding: utf-8 -*-

"""Convert Python AST nodes to Erlang Abstract Format

Python doc:
    http://docs.python.org/library/compiler.html#compiler.ast.Node
Erlang doc:
    http://www.erlang.org/doc/apps/erts/absform.html

Example.

def test(a, b):
    return a + b

        ||
        \/

[{attribute,1,module,sample},
 {attribute,2,export,[{add,2}]},
    {function,4,add,2,
        [{clause,4,[{integer,4,0},{integer,4,0}],[],[{integer,5,0}]},
            {clause,6,
                [{var,6,'A'},{var,6,'B'}],
                [],
                [{op,7,'+',{var,7,'A'},{var,7,'B'}}]}]},

"""

from errors import AbstractFormError

def atom_af(name, lineno=1):
    fmt = locals()
    return '{{atom, {lineno}, {name}}}'.format(**fmt)

def variable_af(varname, lineno=1):
    """Variable name. Require further values binding."""
    # Erlang variables always start from Capital letter
    varname = varname.capitalize()
    fmt = locals()
    return "{{var, {lineno}, '{varname}'}}".format(**fmt)

def term_af(val, lineno=1):
    """Variable value"""
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
    """Addition operation"""
    return _binary_op_af('+', left, right, lineno)

def substraction_af(left, right, lineno=1):
    """Substraction operation"""
    return _binary_op_af('-', left, right, lineno)

def multiplication_af(left, right, lineno=1):
    """Multiplication operation"""
    return _binary_op_af('*', left, right, lineno)

def division_af(left, right, lineno=1):
    """Division operation"""
    return _binary_op_af('/', left, right, lineno)

def _binary_op_af(op, left, right, lineno=1):
    """Abstract form of binary operations"""
    fmt = locals()
    return "{{op, {lineno}, '{op}', {left}, {right}}}".format(**fmt)

def _clause_af(arguments, body, lineno=1):
    """Function claue:
    f(0) -> 0;      % clause 1
    f(N) -> N * N.  % clause 2
    """
    arguments = ','.join([variable_af(arg) for arg in arguments])
    fmt = locals()
    return '{{clause, {lineno}, [{arguments}], [], [{body}]}}'.format(**fmt)

def function_af(fn_name, argnames, clauses, lineno=1):
    """Function with one clause"""
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
    """Assemble module items"""
    fmt = locals()
    fmt['exports'] = ','.join(exports)
    fmt['functions'] = ','.join(functions)
    return '[{module}, {exports}, {functions}]'.format(**fmt)

def _fn_call_af(module, fn_name, args, lineno=1):
    fmt = locals()
    return '{{call, {lineno}, {{remote, {lineno}, {module},'\
            '{fn_name}}}, [{args}]}}'.format(**fmt)

def io_format_af(items, lineno=1):
    items = ','.join(items)
    return _fn_call_af(atom_af('io'), atom_af('format'), items, lineno)

def nil_af(lineno=1):
    return '{{nil, {lineno}}}'.format(lineno=lineno)
