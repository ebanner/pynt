#!/usr/bin/env python

import ast

import astor
import codebook.node_transformers
from codebook.node_transformers import (DeepAnnotator, DefunFinder,
                                        ExpressionFinder, FirstPassForSimple,
                                        IPythonEmbedder, NamespacePromoter,
                                        ShallowAnnotator, SyntaxRewriter,
                                        UnpackIf)


def find_func(module, namespace):
    """Filter away everything except the function

    Addionally rename the function for better readability.

    Args:
        module (ast.Module): the entire parsed code
        namespace (str): identifier for the function of interest

    `namspace` will be of the form <module_name>.<function_name>

    Returns:
        module (ast.Module): the original module but with everything filtered
        away except the function and the function with a more readable name

    """
    module_name, func_name = namespace.split('.')
    funcs = [stmt for stmt in module.body if isinstance(stmt, ast.FunctionDef)]
    func, = [func for func in funcs if func.name == func_name]
    func.name = f'{module_name}.{func_name}'
    module.body = [func]
    return module

def find_method(module, namespace):
    """Filter away everything except the method

    Promote the method up to the global namespace so that it is
    indistinguishable from a regular function.

    Arguments:
        module (ast.Module): the entire parsed source code
        namespace (str): identifier for the method of interest

    Returns:
        module (ast.Module): the original module but with everything filtered
        away except the method name but with the name `namespace` and promoted
        to the global (i.e. top) level

    """
    module_name, class_name, method_name = namespace.split('.')
    classdefs = [stmt for stmt in module.body if isinstance(stmt, ast.ClassDef)]
    classdef, = [classdef for classdef in classdefs if classdef.name == class_name]
    methods = [stmt for stmt in classdef.body if isinstance(stmt, ast.FunctionDef)]
    for method in methods:
        if method.name == method_name:
            method.name = f'{module_name}.{class_name}.{method_name}'
            module.body = [method]
            return module

def filter_away_except(tree, namespace):
    """Filter away everything except the namespace

    In the case that `namespace` is a method promote the method to the global
    (i.e. top) level.

    Arguments:
        tree (ast.Module): the entire parsed code from the code buffer
        namespace (str): the namespace (i.e. region of code) of interest

    Returns:
        small_tree (ast.Module): `tree` but everything filtered except the
        namespace region

    """
    ns_tokens = namespace.split('.')
    if len(ns_tokens) == 1: # module-level
        tree.body = [stmt for stmt in tree.body if not isinstance(stmt, ast.FunctionDef) and not isinstance(stmt, ast.ClassDef)]
        e = codebook.node_transformers.make_annotation(buffer=namespace, content=f'`{namespace}`', cell_type='1')
        tree.body.insert(0, e)
        small_tree = tree
    elif len(ns_tokens) == 2: # function
        small_tree = find_func(tree, namespace)
    else: # method
        assert len(ns_tokens) == 3
        small_tree = find_method(tree, namespace)
    return small_tree

def annotate(code, namespace, shallow=False):
    """Annotate region with code to make and eval cells

    Arguments:
        code (str): the code from the code buffer
        namespace (str): the namespace identifying the code region of interest

    `namespace` can take one of three forms depending on the type of code
    region of interest:

        - "<module_name>"
        - "<module_name>.<func_name>"
        - "<module_name>.<class_name>.<method_name>"

    """
    tree = ast.parse(code)
    small_tree = filter_away_except(tree, namespace)
    shallow_tree = NamespacePromoter(buffer=namespace).visit(tree)

    if shallow:
        annotations = ShallowAnnotator(namespace).visit(shallow_tree)
        return unpack_annotations(annotations)
    else:
        annotations = DeepAnnotator(namespace).visit(shallow_tree)
        new_code = astor.to_source(annotations)
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
        DefunFinder(func_name, lineno).visit(tree)
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

def unpack_annotations(annotations):
    """Extract the information out of a bunch of annotations

    >>> code = '''
    ...
    ... __cell__('`foo.baz`', 'foo.baz', '1', '9')
    ... __cell__('Arguments', 'foo.baz', '1', '-1')
    ... __cell__('Body', 'foo.baz', '1', '-1')
    ... __cell__('pass', 'foo.baz', 'code', '10')
    ...
    ... '''
    ...
    >>> annotations = ast.parse(code)

    """
    info = []
    m = astor.to_source(annotations)
    print(m)
    for annotation in annotations.body:
        content, namespace, cell_type, lineno = [arg.s for arg in annotation.value.args]
        info.append([content, namespace, cell_type, int(lineno)])
    return info

def expand_loop(code, namespace, lineno):
    """Extract the information out of a bunch of annotations

    >>> code = '''
    ...
    ... for i in range(5):
    ...     print(i)
    ...
    ... '''
    >>>
    >>> namespace = 'foo'
    >>> lineno = 3

    """
    tree = ast.parse(code)
    small_tree = filter_away_except(tree, namespace)
    shallow_tree = NamespacePromoter(buffer=namespace).visit(small_tree)
    small_shallow_tree = ExpressionFinder(lineno).visit(shallow_tree)
    unrolled_for = FirstPassForSimple(namespace).visit(small_shallow_tree)
    annotations = ShallowAnnotator(namespace).visit(unrolled_for)
    return unpack_annotations(annotations)

def unpack_if(code, namespace, lineno):
    """Extract the information out of a bunch of annotations

    >>> code = '''
    ...
    ... if 1:
    ...     if 2:
    ...         print(12)
    ...
    ... '''
    >>>
    >>> namespace = 'foo'
    >>> lineno = 3

    """
    tree = ast.parse(code)
    small_tree = filter_away_except(tree, namespace)
    shallow_tree = NamespacePromoter(buffer=namespace).visit(small_tree)
    small_shallow_tree = ExpressionFinder(lineno).visit(shallow_tree)
    unrolled_if = UnpackIf(namespace).visit(small_shallow_tree)
    annotations = ShallowAnnotator(namespace).visit(unrolled_if)
    return unpack_annotations(annotations)

def unpack(code, namespace, lineno):
    """Extract the information out of a bunch of annotations

    >>> code = '''
    ...
    ... if 1:
    ...     if 2:
    ...         print(12)
    ...
    ... '''
    >>>
    >>> namespace = 'foo'
    >>> lineno = 3

    """
    tree = ast.parse(code)
    small_tree = filter_away_except(tree, namespace)
    shallow_tree = NamespacePromoter(buffer=namespace).visit(small_tree)
    small_shallow_tree = ExpressionFinder(lineno).visit(shallow_tree)
    expr_type, = small_shallow_tree.body
    if isinstance(expr_type, ast.For):
        unpacked = FirstPassForSimple(namespace).visit(small_shallow_tree)
    else:
        assert isinstance(expr_type, ast.If)
        unpacked = UnpackIf(namespace).visit(small_shallow_tree)
    annotations = ShallowAnnotator(namespace).visit(unpacked)
    return unpack_annotations(annotations)
