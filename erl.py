# -*- coding: utf-8 -*-

'''Function which convert python AST nodes into Erlang abstract form'''

def name_form(val):
    """Pythons' variables (variable name)"""
    return "{var, 1, '%s'}" % val.upper()

def const_form(val):
    """Represent Python constant, e.g. 10, "string", 23.23"""
    return '{integer, 1, %s}' % val

def add_form(p1, p2):
    """Add operator"""
    return "{op, 1, '+', %s, %s}" % (p1, p2)

def function_form(name, argnames, clauses):
    params = ','.join([name_form(arg) for arg in argnames])
    return "{function, 1, '%(name)s', %(args_num)s, [{clause, 1, [%(params)s], [], %(clause)s}] }" % {
            'name': name, 'args_num':len(argnames), 'params': params, 'clause': clauses
        }

def stmt_form(nodes):
    return '[%s]' % ','.join(nodes)

def return_form(nodes):
    return ','.join(nodes)
