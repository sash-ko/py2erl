# -*- coding: utf-8 -*-

class AbstractFormError(Exception):

    def __init__(self, valtype, val):
        self.value = val
        self.valtype = valtype

    def __str__(self):
        return 'Can not build abstract form for {0} ({1})'.format(self.valtype,
                                                                self.value)

class NotSupportedNode(Exception):

    def __init__(self, node_klass):
        self.node_klass = node_klass

    def __str__(self):
        return "'{0}' node is not supported".format(self.node_klass)
