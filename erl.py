# -*- coding: utf-8 -*-

"""Printable primitives of Erlang AST: attributes, atoms, lists, functions, etc.
"""

import logging

class Primitives(object):
    pass

class Attribute(Primitives):

    def __init__(self, name, arguments):
        self.name = name
        self.arguments = arguments

    def __str__(self):
        args = ','.join([str(arg) for arg in self.arguments])
        return 'erl_syntax:attribute(%s, [%s])' % (str(self.name), args)

class Atom(Primitives):

    def __init__(self, name):
        self.name = name

    def __str__(self):
        return 'erl_syntax:atom("%s")' % self.name

class List(Primitives):

    def __init__(self, elements, tail):
        self.elements = elements
        self.tail = tail

    def __str__(self):
        elements = ','.join([str(el) for el in self.elements])
        return 'erl_syntax:list(%s)' % elements

class Integer(Primitives):

    def __init__(self, value):
        self.value = value

    def __str__(self):
        return 'erl_syntax:integer(%s)' % self.value

class ArityQualifier(Primitives):

    def __init__(self, name, arity):
        self.name = name
        self.arity = arity

    def __str__(self):
        arity = ','.join((str(self.name), str(self.arity)))
        return 'erl_syntax:arity_qualifier(%s)' % arity

def create_module(name):
    """Create AST which represent module.
    Erlang code:
        ModAST = erl_syntax:attribute(erl_syntax:atom(module),
                                       [erl_syntax:atom("mod_name")])
    """
    return Attribute('module', [Atom(name)])

def create_export(name, arity):
    """Create AST which represent all exports.
    Erlang code:
        ExpAST = erl_syntax:attribute(erl_syntax:atom(export),
                                            [erl_syntax:list(
                                                [erl_syntax:arity_qualifier(
                                                    erl_syntax:atom("fn_name"),
                                                    erl_syntax:integer(0))])])
    """
    return Attribute('export', [List([ArityQualifier(Atom(name),
                                                    Integer(arity))], None)])

def create_function(name, clauses):
    """Create functions AST.
    Erlang code:
        FuncAST = erl_syntax:function(erl_syntax:atom("fn_name"),
                                        [erl_syntax:clause(
                                            %....
                                            )])
    Not implemented.
    """
    logging.warning("Function convertion is not implemented")

def compose_ast(module, exports, functions):
    return compile_template % {'mod': str(module),
                               'exp': [str(e) for e in exports],
                               'func': [str(e) for e in functions]}

compile_template = '''
-module(gen).
-export([make/0]).

make() ->
    ModAST = %(mod)s,
    ExpAST = %(exp)s,
    FuncsAST = %(func)s,

    Items = [ModAST] ++ ExpAST ++ FuncsAST,
    Forms = [erl_syntax:revert(AST) || AST <- Items],

    case compile:forms(Forms) of
        {ok,ModuleName,Binary} -> code:load_binary(ModuleName, "gen", Binary);
        {ok,ModuleName,Binary,_Warnings} -> code:load_binary(ModuleName, "gen", Binary)
    end.
'''
