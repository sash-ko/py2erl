Primitive and naive Python to Erlang compiler.

Usage: 
    python py2erl.py python_file.py
    erl
    code:load_file(python_file)
    python_file:fn_name(1, 2)


Idea: python code -> Python Abstract Syntax Tree -> Erlang abstract format -> compile

Docs:
    http://docs.python.org/library/compiler.html
    http://docs.python.org/library/ast.html
    http://www.erlang.org/doc/apps/erts/absform.html
    http://www.erlang.org/doc/man/erl_syntax.html
    http://www.trapexit.org/String_Eval
    http://www.scribd.com/doc/22451864/Hacking-Erlang
    http://www.scribd.com/doc/18149479/Domain-Specific-Languages-in-Erlang
    http://www.infoq.com/articles/erlang-dsl
    http://johnbender.us/2009/04/22/searching-the-abstract-form-in-erlang/
