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
    ...     print('foo!')
    ...
    ... def bar():
    ...     print('bar!')
    ...
    ... def biz():
    ...     print('biz!')
    ... '''
    >>> code = [s, 'N/A']

    """
    code, active_funcname = code[0], code[1]
    tree = ast.parse(code)
    new_code = str()
    if active_funcname == 'N/A': # just create worksheets
        func_names = [stmt.name for stmt in tree.body if isinstance(stmt, ast.FunctionDef)]
        func_names = ['outside'] + func_names
        buffer_names = reversed([f'context={func_name}' for func_name in func_names])
        commands = [f'__cell__("pass", "{buffer_name}", "code", "-1")' for buffer_name in buffer_names]
        new_code = '\n'.join(commands)
    else:
        if active_funcname == 'outside':
            tree.body = [stmt for stmt in tree.body if not isinstance(stmt, ast.FunctionDef)]
        else:
            tree.body = [stmt for stmt in tree.body if isinstance(stmt, ast.FunctionDef) and stmt.name == active_funcname]

        # apply transformations
        tree = FunctionExploder().visit(tree)
        tree = SyntaxRewriter(buffer=active_funcname).visit(tree)
        tree = Annotator(buffer=active_funcname).visit(tree)
        new_code = astor.to_source(tree)

    return new_code

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
