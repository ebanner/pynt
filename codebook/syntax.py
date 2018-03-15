#!/usr/bin/env python

import ast

import astor

from codebook.node_transformers import (Annotator, FirstPassForSimple,
                                        FunctionExploder, IPythonEmbedder,
                                        LineNumberFinder, SyntaxRewriter)


def annotate(*region):
    """Annotate region with code to make and eval cells

    Args:
        s (str): the code
        ns (str): the namespace

    `ns` is of the form


        - <module-name>
        - <module-name>.<func-name>
        - <module-name>.<class-name>.<method-name>

    region = [s, ns]

    >>> code = '''
    ...
    ... x
    ... def bar():
    ...     pass
    ... class Biz:
    ...     def baz(self):
    ...         pass
    ... def qux():
    ...     pass
    ... y
    ...
    ... '''
    >>>
    >>> namespace = 'foo.bar'
    >>> region = [code, namespace]

    """
    code, namespace = region
    tree = ast.parse(code)
    ns_tokens = namespace.split('.')
    if len(ns_tokens) == 1: # top-level
        tree.body = [stmt for stmt in tree.body if not isinstance(stmt, ast.FunctionDef) and not isinstance(stmt, ast.ClassDef)]
        e = Annotator.make_annotation(buffer=namespace, content=f'`{namespace}`', cell_type='1')
        tree.body.insert(0, e)
    elif len(ns_tokens) == 2: # function
        module_name, func_name = ns_tokens
        funcs = [stmt for stmt in tree.body if isinstance(stmt, ast.FunctionDef)]
        func, = [func for func in funcs if func.name == func_name]
        func.name = f'{module_name}.{func_name}'
        tree.body = [func]
    else: # method
        assert len(ns_tokens) == 3
        module_name, class_name, method_name = ns_tokens
        classdefs = [stmt for stmt in tree.body if isinstance(stmt, ast.ClassDef)]
        classdef, = [classdef for classdef in classdefs if classdef.name == class_name]
        methods = [stmt for stmt in classdef.body if isinstance(stmt, ast.FunctionDef)]
        for method in methods:
            if method.name == method_name:
                method.name = namespace # rename method for readability
                tree.body = [method]
                break

    exploded_tree = FunctionExploder(buffer=namespace).visit(tree)
    # rewritten_tree = SyntaxRewriter(buffer=namespace).visit(exploded_tree)
    annotated_tree = Annotator(buffer=namespace).visit(exploded_tree)
    new_code = astor.to_source(annotated_tree)

    return new_code

def parse_namespaces(*region):
    """Parse namespaces out of the code

    Returns:
        namespaces (list): a list of 3-tuples = [
            (namespace, start-line, end-line),
            (namespace, start-line, end-line),
            .
            .
            .
            (namespace, start-line, end-line),
        ]

    >>> s = '''
    ...
    ... x
    ... class Foo:
    ...     def bar():
    ...         \"\"\"function\"\"\"
    ...         pass
    ...     def biz():
    ...         \"\"\"function\"\"\"
    ...         pass
    ...
    ... class Qux:
    ...     def quux():
    ...         \"\"\"function\"\"\"
    ...         pass
    ...     def quuux():
    ...         \"\"\"function\"\"\"
    ...         pass
    ... y
    ...
    ... '''
    >>> region = [s, 'ast-server']

    """
    code, namespace = region
    tree = ast.parse(code)
    new_code = str()
    module_name = namespace
    funcs = [stmt for stmt in tree.body if isinstance(stmt, ast.FunctionDef)]
    methods = []
    classdefs = [stmt for stmt in tree.body if isinstance(stmt, ast.ClassDef)]
    for classdef in classdefs:
        for expr in classdef.body:
            if not isinstance(expr, ast.FunctionDef):
                continue
            methods.append([classdef, expr])
    namespaces = \
        [(ns, ns, -1, -1) for ns in [f'{module_name}']] + \
        [(ns, ns, func.lineno, func.body[-1].lineno) for func in funcs for ns in [f'{module_name}.{func.name}']] + \
        [(ns, ns, method.lineno, method.body[-1].lineno) for classdef, method in methods for ns in [f'{module_name}.{classdef.name}.{method.name}']]
    return namespaces

def embed(*region):
    """Replace the function or method with a call to `IPython.embed()`

    Args:
        s (str): the code
        ns (str): the namespace

    `ns` is of the form

        - <module-name>.<func-name>
        - <module-name>.<class-name>.<method-name>

    region = [s, ns]

    >>> s = '''
    ...
    ... x
    ... class Foo:
    ...     def bar():
    ...         \"\"\"function\"\"\"
    ...         pass
    ...     def biz():
    ...         \"\"\"function\"\"\"
    ...         pass
    ...
    ... class Qux:
    ...     def quux():
    ...         \"\"\"function\"\"\"
    ...         pass
    ...     def quuux():
    ...         \"\"\"function\"\"\"
    ...         pass
    ... y
    ...
    ... '''
    >>>
    >>> namespace = 'ast_server.Qux.quux'
    >>> region = [s, namespace]

    """
    code, namespace = region
    tree = ast.parse(code)
    embedded = IPythonEmbedder(namespace).visit(tree)
    c = astor.to_source(embedded)
    return c

def find_namespace(code, func_name, lineno):
    """Compute the fully qualified namespace of `func_name` at `lineno` from `code`

    Args:
        code (str): the code
        func_name (str): the function name
        lineno (str): the line that `func_name` is defined at in `code`

    Returns:
        A namespace string

        - <func-name>
        - <class-name>.<method-name>

    >>> code = '''
    ...
    ... x
    ... class Foo:
    ...     def bar():
    ...         \"\"\"function\"\"\"
    ...         pass
    ...     def biz():
    ...         \"\"\"function\"\"\"
    ...         pass
    ...
    ... class Qux:
    ...     def bar():
    ...         \"\"\"function\"\"\"
    ...         pass
    ...     def biz():
    ...         \"\"\"function\"\"\"
    ...         pass
    ... y
    ...
    ... '''
    >>>
    >>> func_name = 'bar'
    >>> lineno = 5

    """
    namespace = None
    try:
        LineNumberFinder(func_name, lineno).visit(tree)
    except Exception as e:
        namespace, = e.args
    return namespace

def promote_loop(*region):
    """

    >>> code = '''
    ...
    ... for i in range(5):
    ...     for j in range(5):
    ...         k = i = j
    ...         print(k)
    ...
    ... '''

    """
    code, namespace = region
    tree = ast.parse(code)
    m = FirstPassForSimple(buffer=namespace).visit(tree)
    return astor.to_source(m)

def annotate_toplevel(*region):
    """

    >>> code = '''
    ...
    ... for i in range(5):
    ...     for j in range(5):
    ...         k = i = j
    ...         print(k)
    ...
    ... '''
    >>> namespace = 'foo'
    >>> region = [code, namespace]

    """
    code, namespace = region
    tree = ast.parse(code)
    annotated_tree = Annotator(buffer=namespace).visit(tree)
    return astor.to_source(annotated_tree)
