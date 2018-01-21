import ast
import doctest
import logging
logging.basicConfig(level=logging.INFO)

import astor
from epc.server import EPCServer
from node_transformers import Annotator, FunctionExploder, SyntaxRewriter

server = EPCServer(('localhost', 0))


@server.register_function
def annotate(*code):
    """Annotate code with code to make and eval cells

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
    >>> code = [s, '*ast-server*']

    """
    code, namespace = code[0], code[1]
    tree = ast.parse(code)
    ns_tokens = namespace.split('.')
    if len(ns_tokens) == 1: # top-level
        tree.body = [stmt for stmt in tree.body if not isinstance(stmt, ast.FunctionDef) and not isinstance(stmt, ast.ClassDef)]
    elif len(ns_tokens) == 2: # function
        module_name, func_name = ns_tokens
        funcs = [stmt for stmt in tree.body if isinstance(stmt, ast.FunctionDef)]
        tree.body = [func for func in funcs if func.name == func_name]
    else: # method
        assert len(ns_tokens) == 3
        module_name, class_name, method_name = ns_tokens
        classdefs = [stmt for stmt in tree.body if isinstance(stmt, ast.ClassDef)]
        for classdef in classdefs:
            methods = [stmt for stmt in classdef.body if isinstance(stmt, ast.FunctionDef)]
            for method in methods:
                if method.name == method_name:
                    method.name = namespace # rename method for readability
                    tree.body = [method]
                    break

    exploded_tree = FunctionExploder(buffer=namespace).visit(tree)
    rewritten_tree = SyntaxRewriter(buffer=namespace).visit(exploded_tree)
    annotated_tree = Annotator(buffer=namespace).visit(rewritten_tree)
    new_code = astor.to_source(annotated_tree)

    return new_code

@server.register_function
def parse_namespaces(*code):
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
    >>> code = [s, '*ast-server*']

    """
    code, namespace = code[0], code[1]
    tree = ast.parse(code)
    new_code = str()
    star, module_name, star = namespace.split('*')
    funcs = [stmt for stmt in tree.body if isinstance(stmt, ast.FunctionDef)]
    methods = []
    classdefs = [stmt for stmt in tree.body if isinstance(stmt, ast.ClassDef)]
    for classdef in classdefs:
        for expr in classdef.body:
            if not isinstance(expr, ast.FunctionDef):
                continue
            methods.append(expr)
    namespaces = \
        [(f'ns={module_name}', -1, -1)] + \
        [(f'ns={module_name}.{func.name}', func.lineno, func.body[-1].lineno) for func in funcs] + \
        [(f'ns={module_name}.{classdef.name}.{expr.name}', method.lineno, method.body[-1].lineno) for method in methods]
    namespaces = list(reversed(namespaces))
    return namespaces

if __name__ == '__main__':
    server.print_port()
    server.serve_forever()

if __name__ == '__test__':
    code = '''

    def foo(a):
        """This is a docstring

        >>> a = 7

        """
        for i in range(a):
            print(i)

    '''
    active_funcname = 'foo'
    annotated_code = annotate(code, active_funcname)
    print(annotated_code)
