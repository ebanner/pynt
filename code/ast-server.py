import ast
import doctest

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
    >>> code = [s, 'Foo.bar']

    """
    code, context = code[0], code[1]
    tree = ast.parse(code)
    new_code = str()
    if context == 'N/A': # just create worksheets
        func_names = [stmt.name for stmt in tree.body if isinstance(stmt, ast.FunctionDef)]
        method_names = []
        classdefs = [stmt for stmt in tree.body if isinstance(stmt, ast.ClassDef)]
        for classdef in classdefs:
            for expr in classdef.body:
                if not isinstance(expr, ast.FunctionDef):
                    continue
                method_names.append(f'{classdef.name}.{expr.name}')
        contexts = ['outside'] + func_names + method_names
        buffer_names = reversed([f'context={func_name}' for func_name in contexts])
        commands = [f'__cell__("pass", "{buffer_name}", "code", "-1")' for buffer_name in buffer_names]
        new_code = '\n'.join(commands)
    else:
        if context == 'outside':
            tree.body = [stmt for stmt in tree.body if not isinstance(stmt, ast.FunctionDef)]
        elif '.' in context: # method
            func_name = context.split('.')[-1]
            classdefs = [stmt for stmt in tree.body if isinstance(stmt, ast.ClassDef)]
            for classdef in classdefs:
                methods = [stmt for stmt in classdef.body if isinstance(stmt, ast.FunctionDef)]
                for method in methods:
                    if method.name == func_name:
                        method.name = context # rename method for readability
                        tree.body = [method]
                        break
        else:
            func_name = context
            funcs = [stmt for stmt in tree.body if isinstance(stmt, ast.FunctionDef)]
            tree.body = [func for func in funcs if func.name == context]

        # apply transformations
        tree = FunctionExploder().visit(tree)
        tree = SyntaxRewriter(buffer=context).visit(tree)
        tree = Annotator(buffer=context).visit(tree)
        new_code = astor.to_source(tree)

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
