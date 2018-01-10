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
    ... def foo():
    ...     \"\"\"function\"\"\"
    ...     print('foo!')
    ...
    ... def bar():
    ...     print('bar!')
    ...
    ... def biz():
    ...     print('biz!')
    ... '''
    >>> code = [s, 'foo']
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
    ... x
    ...
    ... '''
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
    >>> code = [s, 'ast-server']
    >>> code = [s, 'Foo.bar']
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
    ... x
    ...
    ... '''
    >>> code = [s, '*ast-server*']

    """
    code, namespace = code[0], code[1]
    tree = ast.parse(code)
    new_code = str()
    if namespace.startswith('*') and namespace.endswith('*'): # just create worksheets
        star, module_name, star = namespace.split('*')
        func_names = [f'{module_name}.{stmt.name}' for stmt in tree.body if isinstance(stmt, ast.FunctionDef)]
        method_names = []
        classdefs = [stmt for stmt in tree.body if isinstance(stmt, ast.ClassDef)]
        for classdef in classdefs:
            for expr in classdef.body:
                if not isinstance(expr, ast.FunctionDef):
                    continue
                method_names.append(f'{module_name}.{classdef.name}.{expr.name}')
        namespaces = [module_name] + func_names + method_names
        buffer_names = reversed([f'ns={namespace}' for namespace in namespaces])
        commands = [f'__cell__("pass", "{buffer_name}", "code", "-1")' for buffer_name in buffer_names]
        new_code = '\n'.join(commands)
    else:
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

        # apply transformations
        logging.info('This is how to the looks before applying node transformations = ')
        print(astor.to_source(tree))
        logging.info('Exploding functions...')
        tree = FunctionExploder(buffer=namespace).visit(tree)
        logging.info(astor.to_source(tree))
        logging.info('Rewriting syntax...')
        tree = SyntaxRewriter(buffer=namespace).visit(tree)
        logging.info(astor.to_source(tree))
        logging.info('done!')
        logging.info('Annotating code...')
        logging.info('done!')
        tree = Annotator(buffer=namespace).visit(tree)
        logging.info('Converting AST to source...')
        new_code = astor.to_source(tree)
        logging.info(astor.to_source(tree))
        logging.info('done!')

    return new_code

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
