"""Node transformers for manipulating the abstract syntax tree

Transformers for exploding functions, rewriting syntax, and adding annotations
exist currently.

"""

import ast
import copy
import doctest
import random
import string

import astor

N = []
def __random_string__():
    """Compute a random string

    Also cache them in the global stack `N` so we can get them out later.

    """
    n = ''.join(random.choice(string.ascii_lowercase) for _ in range(10))
    global N
    N.append(n)
    return n

def upcase(s):
    """

    >>> s = 'foo'

    """
    u = f'{s[0].upper()}{s[1:]}'
    return u

def make_annotation(node=None, buffer='outside', content=None, cell_type='code', lineno=None):
    """Return a ast.Expr that looks like

    ```
    __cell__('make-cell', [content, buffer, cell_type])
    ```

    """
    content = astor.to_source(node).strip() if node else content
    lineno = str(node.lineno) if hasattr(node, 'lineno') else str(-1) if not lineno else str(lineno)
    call = ast.Call(
        func=ast.Name(id='__cell__', ctx=ast.Load()),
        args=[
            ast.Str(s=content),
            ast.Str(s=f'{buffer}'),
            ast.Str(s=cell_type),
            ast.Str(s=lineno),
        ],
        keywords=[]
    )
    return ast.Expr(call)

class ExpressionFinder(ast.NodeTransformer):
    """Find the expression which contains the line number"""

    def __init__(self, lineno):
        super(__class__, self).__init__()
        self.lineno = lineno

    def visit_Module(self, module):
        """Search the module's top-level expressions

        >>> self = ExpressionFinder(lineno=4)
        >>> code = '''
        ...
        ... a
        ... b
        ... c
        ...
        ... '''
        >>>
        >>> module = ast.parse(code)

        """
        for i, expr in enumerate(module.body):
            if getattr(expr, 'lineno', -float('inf')) < self.lineno:
                continue
            elif expr.lineno == self.lineno:
                module.body = [expr]
            else:
                assert expr.lineno > self.lineno
                module.body = module.body[i-1:i]
            break
        else:
            module.body = module.body[-1:]
        return module

class DefunFinder(ast.NodeTransformer):
    """Find the function or method which is defined at a particular line number"""

    def __init__(self, func_name, lineno):
        """

        >>> self = DefunFinder.__new__(DefunFinder)
        >>> __class__ = DefunFinder
        >>> func_name = 'bar'
        >>> lineno = 5

        """
        super(__class__, self).__init__()
        self.func_name = func_name
        self.lineno = lineno

    def visit_ClassDef(self, classdef):
        """Check the line number of each of the methods

        >>> self = DefunFinder(func_name='bar', lineno=4)
        >>> code = '''
        ...
        ... class Foo:
        ...     def bar():
        ...         \"\"\"function\"\"\"
        ...         pass
        ...     def biz():
        ...         \"\"\"function\"\"\"
        ...         pass
        ...
        ... '''
        >>>
        >>> tree = ast.parse(code)
        >>> classdef = tree.body[0]

        """
        methods = [stmt for stmt in classdef.body if isinstance(stmt, ast.FunctionDef)]
        for method in methods:
            if method.name == self.func_name and method.lineno == self.lineno:
                raise Exception(f'{classdef.name}.{method.name}')
        return classdef

    def visit_FunctionDef(self, func):
        """Embed a `IPython.embed_kernel()` call into the function

        >>> self = DefunFinder(func_name='bar', lineno=4)
        >>> code = '''
        ...
        ... class Foo:
        ...     def bar():
        ...         \"\"\"function\"\"\"
        ...         pass
        ...     def biz():
        ...         \"\"\"function\"\"\"
        ...         pass
        ...
        ... '''
        >>>
        >>> tree = ast.parse(code)
        >>> classdef = tree.body[0]

        """
        if func.name == self.func_name and func.lineno == self.lineno:
            raise Exception(func.name)
        return func

class IPythonEmbedder(ast.NodeTransformer):
    """Replaces the body of a function with `IPython.embed_kernel()`.

    Specifically swap out the body of a function with a call to fork off the
    `IPython.embed_kernel()` call.

    """
    def __init__(self, namespace):
        """

        >>> self = IPythonEmbedder.__new__(IPythonEmbedder)
        >>> namespace = 'foo.bar'
        >>> __class__ = IPythonEmbedder

        """
        super(__class__, self).__init__()
        self.namespace = namespace
        tokens = namespace.split('.')
        if len(tokens) == 1:
            self.module, = tokens
            self.func_type = 'module'
        elif len(tokens) == 2:
            self.module, self.func_name = tokens
            self.func_type = 'function'
        else:
            assert len(tokens) == 3
            self.module, self.class_name, self.func_name = tokens
            self.func_type = 'method'

    @staticmethod
    def get_kernel_embed():
        """A list of kernel embed nodes

        Returns:
            nodes (list): AST nodes which form the following code.

            ```
            import os
            if os.fork() > 0:
                import time
                time.sleep(1)
                os._exit(0)
            open(f'{os.environ["HOME"]}/.pynt', 'a').close()
            import IPython
            IPython.start_kernel(user_ns={**locals(), **globals(), **vars()})
            ```

        This is a purely functional method which always return the same thing.

        """
        return [
            ast.Import(names=[ast.alias(name='os', asname=None),]),
            ast.Assign(targets=[ast.Name(id='pid', ctx=ast.Store()),], value=ast.Call(func=ast.Attribute(value=ast.Name(id='os', ctx=ast.Load()), attr='fork', ctx=ast.Load()), args=[], keywords=[])),
            ast.If(
                test=ast.Compare(left=ast.Name(id='pid', ctx=ast.Load()), ops=[ast.Eq(),], comparators=[ast.Num(n=0),]),
                body=[
                    ast.Expr(value=ast.Call(func=ast.Attribute(value=ast.Call(func=ast.Name(id='open', ctx=ast.Load()), args=[
                        ast.JoinedStr(values=[
                            ast.FormattedValue(value=ast.Subscript(value=ast.Attribute(value=ast.Name(id='os', ctx=ast.Load()), attr='environ', ctx=ast.Load()), slice=ast.Index(value=ast.Str(s='HOME')), ctx=ast.Load()), conversion=-1, format_spec=None),
                            ast.Str(s='/.pynt'),
                        ]),
                        ast.Str(s='a'),
                    ], keywords=[]), attr='close', ctx=ast.Load()), args=[], keywords=[])),
                    ast.Import(names=[
                        ast.alias(name='IPython', asname=None),
                    ]),
                    ast.Expr(value=ast.Call(func=ast.Attribute(value=ast.Name(id='IPython', ctx=ast.Load()), attr='start_kernel', ctx=ast.Load()), args=[], keywords=[
                        ast.keyword(arg='user_ns', value=ast.Dict(keys=[
                            None,
                            None,
                            None,
                        ], values=[
                            ast.Call(func=ast.Name(id='locals', ctx=ast.Load()), args=[], keywords=[]),
                            ast.Call(func=ast.Name(id='globals', ctx=ast.Load()), args=[], keywords=[]),
                            ast.Call(func=ast.Name(id='vars', ctx=ast.Load()), args=[], keywords=[]),
                        ])),
                    ])),
            ], orelse=[]),
            ast.Expr(value=ast.Call(func=ast.Attribute(value=ast.Name(id='os', ctx=ast.Load()), attr='waitpid', ctx=ast.Load()), args=[
                ast.Name(id='pid', ctx=ast.Load()),
                ast.Num(n=0),
            ], keywords=[])),
        ]

    def visit_Module(self, module):
        """Maybe replace the entire module with a kernel

        If namespace is targeting the top-level then we do it.

        >>> self = IPythonEmbedder(namespace='foo.foo')
        >>> code = '''
        ...
        ... import random
        ... def foo():
        ...     pass
        ...
        ... '''
        >>> module = ast.parse(code)

        """
        if self.func_type == 'module':
            module.body = self.get_kernel_embed()
        else:
            module = self.generic_visit(module)
        return module

    def visit_ClassDef(self, classdef):
        """Embed a kernel into classdef.target`

        If either `self.func_type` is a function or `self.class_name` does not
        match this class then that means this is not the classdef you are
        looking for.

        >>> self = IPythonEmbedder(namespace='ast_server.Foo.biz')
        >>> code = '''
        ...
        ... class Foo:
        ...     def bar():
        ...         \"\"\"function\"\"\"
        ...         pass
        ...     def biz():
        ...         \"\"\"function\"\"\"
        ...         pass
        ...
        ... '''
        >>>
        >>> tree = ast.parse(code)
        >>> classdef = tree.body[0]

        """
        if self.func_type == 'function':
            node = classdef
        elif not self.class_name == classdef.name:
            node = classdef
        else:
            assert classdef.name == self.class_name and self.func_type == 'method'
            methods = [stmt for stmt in classdef.body if isinstance(stmt, ast.FunctionDef)]
            [idx, method], = [(i, method) for i, method in enumerate(methods) if method.name == self.func_name]
            classdef.body[idx] = self.visit_FunctionDef(method)
            node = classdef
        return node

    def visit_FunctionDef(self, func):
        """Embed a `IPython.embed_kernel()` call into the function

        Recall the context this node visitor is running in is that we are
        embedding a function. Because of the existence of `visit_ClassDef()`
        the only time we will visit a method is when we are called directly on
        the method that needs to be embedded. Hence it is sufficient to just
        check that `func.name == self.func_name` with no risk that we will
        embed a method which has the same name as the target method but is in a
        different class.

        >>> self = IPythonEmbedder(namespace='ast_server.foo')
        >>> code = '''
        ...
        ... x
        ... def foo():
        ...     x = 1
        ...     y = 2
        ...     z = x + y
        ...     return z
        ... y
        ...
        ... '''
        >>> tree = ast.parse(code)
        >>> func = tree.body[1]

        """
        if not func.name == self.func_name:
            node = func
        else:
            func.body = self.get_kernel_embed()
            node = func
        return node

class NamespacePromoter(ast.NodeTransformer):
    """Takes a body of a function and pushes it into the global namespace"""

    def __init__(self, buffer):
        super(__class__, self).__init__()
        self.buffer = buffer

    def visit_Return(self, return_):
        """Convert returns into assignment/exception pairs

        Since the body of this function will be in the global namespace we
        can't have any returns. An acceptable alternative is to set a variable
        called 'RETURN' and then immediately raise an exception.

        >>> self = NamespacePromoter(buffer='foo')
        >>> code = '''
        ...
        ... return 5
        ...
        ... '''
        >>> tree = ast.parse(code)
        >>> return_, = tree.body

        """
        nodes = [
            ast.Assign(targets=[ast.Name(id='RETURN', ctx=ast.Store())], value=return_.value, lineno=return_.lineno),
            ast.Raise(exc=ast.Call(func=ast.Name(id='Exception', ctx=ast.Load()), args=[ast.Str(s='return')], keywords=[]), cause=None),
        ]
        return nodes

    def visit_FunctionDef(self, func):
        """Roll out a function definition

        >>> self = NamespacePromoter(buffer='bar')
        >>> code = '''
        ...
        ... x
        ... def foo(a=1, b=2):
        ...     \"\"\"\Short description
        ...
        ...     Longer description.
        ...
        ...     >>> a = 1
        ...     >>> b = 2
        ...
        ...     "\"\"
        ...     if True:
        ...         return 1
        ...     else:
        ...         return 0
        ... y
        ... '''
        >>> tree = ast.parse(code)
        >>> func = tree.body[1]

        """
        # tranform returns
        func = self.generic_visit(func)

        # extract doctests
        docstring = ast.get_docstring(func, clean=True)
        if docstring:
            func.body = func.body[1:]
            parser = doctest.DocTestParser()
            results = parser.parse(docstring)
            docstring_prefix, docstring_examples = results[0].strip(), [result for result in results if isinstance(result, doctest.Example)]
            docstring_assigns = [example.source.strip() for example in docstring_examples]

        # insert function name and docstring san doctests
        exprs = []
        exprs.append(
            make_annotation(
                buffer=self.buffer,
                content=f'`{func.name}`',
                cell_type='1',
                lineno=func.lineno
            )
        )
        if docstring:
            exprs.append(make_annotation(buffer=self.buffer, content=docstring_prefix, cell_type='markdown'))

        # keyword (default) values
        vars, values = reversed(func.args.args), reversed(func.args.defaults)
        for var, value in zip(vars, values):
            try_ = ast.Try(
                body=[ast.Expr(value=ast.Name(id=var.arg, ctx=ast.Load()))],
                handlers=[
                    ast.ExceptHandler(
                        type=ast.Name(id='NameError', ctx=ast.Load()),
                        name=None,
                        body=[ast.Assign(targets=[ast.Name(id=var.arg, ctx=ast.Store())], value=value)]),
                ],
                orelse=[],
                finalbody=[]
            )
            exprs.append(try_)

        # docstring values override keyword values
        if docstring:
            # exprs.append(make_annotation(buffer=self.buffer, content='Docstring Assignments', cell_type='1'))
            for assign_expr in docstring_assigns:
                tree = ast.parse(assign_expr)
                exprs.append(tree.body[0])

        # final dump of all arguments
        exprs.append(make_annotation(buffer=self.buffer, content='Arguments', cell_type='1'))
        exprs.extend(ast.Expr(arg) for arg in func.args.args)

        exprs.append(make_annotation(buffer=self.buffer, content='Body', cell_type='1'))

        return exprs + func.body

class FirstPassForSimple(ast.NodeTransformer):
    def __init__(self, buffer):
        self.buffer = buffer

    def visit_Continue(self, cont):
        """

        >>> self = FirstPassForSimple('foo')
        >>> code = 'continue'
        >>> module = ast.parse(code)
        >>> cont, = module.body

        """
        return ast.Pass()

    def visit_Break(self, broken):
        """

        >>> self = FirstPassForSimple('foo')
        >>> code = 'break'
        >>> module = ast.parse(code)
        >>> broken, = module.body

        """
        return ast.Pass()

    def visit_For(self, loop):
        """Pure syntax rewrite of a for loop

        Unroll only the first iteration through the loop.

        """
        loop = self.generic_visit(loop)

        # iter(loop.iter)
        iter_call = ast.Call(
            func=ast.Name(id='iter', ctx=ast.Load()),
            args=[ast.Name(id=loop.iter, ctx=ast.Load())],
            keywords=[]
        )

        # i = next(iter(loop.iter))
        get_first = ast.Assign(
            targets=[loop.target],
            value=ast.Call(
                func=ast.Name(id='next', ctx=ast.Load()),
                args=[iter_call],
                keywords=[]
            )
        )
        content = f'`for {astor.to_source(loop.target).strip()} in {astor.to_source(loop.iter).strip()} ...`'
        nodes = []
        nodes.append(make_annotation(buffer=self.buffer, content=content, cell_type='2', lineno=loop.lineno))
        nodes.append(ast.Expr(loop.iter))
        nodes.append(get_first)
        nodes.extend(loop.body)
        return nodes

class FirstPassFor(ast.NodeTransformer):
    """Performs pure syntax rewrites

    Currently the only syntax rewrite are for loops to while loops. Future
    rewrites include context managers and decorators.

    """
    def __init__(self, buffer):
        self.buffer = buffer

    def visit_For(self, loop_):
        """
        >>> self = FirstPassFor(buffer='foo')
        >>> code = '''
        ...
        ... for i in range(5):
        ...     for j in range(5):
        ...         k = i + j
        ...         print(k)
        ...
        ... '''
        >>> tree = ast.parse(code)
        >>> loop_, = tree.body

        """
        loop = self.generic_visit(loop_)
        var = ast.Name(id=__random_string__(), ctx=ast.Store())
        assign = ast.Assign(targets=[var], value=ast.Call(func=ast.Name(id='iter', ctx=ast.Load()), args=[loop.iter], keywords=[]))
        first_pass = ast.Try(
            body=[ast.Assign(targets=[loop.target], value=ast.Call(func=ast.Name(id='next', ctx=ast.Load()), args=[ast.Name(id=var, ctx=ast.Load())], keywords=[]))],
            handlers=[ast.ExceptHandler(type=ast.Name(id='StopIteration', ctx=ast.Load()), name=None, body=[ast.Pass()])],
            orelse=loop.body,
            finalbody=[]
        )
        content = f'`for {astor.to_source(loop.target).strip()} in {astor.to_source(loop.iter).strip()} ...`'
        return [
            make_annotation(buffer=self.buffer, content=content, cell_type='2', lineno=loop.lineno),
            ast.Expr(loop.iter),
            assign,
            first_pass
        ]

class RestIterableFor(ast.NodeTransformer):
    def __init__(self, buffer):
        self.buffer = buffer

    def visit_For(self, loop_):
        """
        >>> self = RestIterableFor()
        >>> code = '''
        ...
        ... for i in range(5):
        ...     for j in range(5):
        ...         k = i + j
        ...         print(k)
        ... '''
        >>> tree = ast.parse(code)
        >>> loop_, = tree.body
        >>> FirstPassFor().visit(copy.deepcopy(loop_))

        """
        loop = self.generic_visit(loop_)
        global N
        varname = N.pop(0)
        loop.iter = ast.Name(id=varname, ctx=ast.Store())
        return loop

class SyntaxRewriter(ast.NodeTransformer):
    """Performs pure syntax rewrites

    Currently the only syntax rewrite are for loops to while loops. Future
    rewrites include context managers and decorators.

    """
    def __init__(self, buffer):
        super(__class__, self).__init__()
        self.buffer = buffer

    def visit_For(self, loop):
        """Rewrite for loops as while loops

        >>> self = SyntaxRewriter(buffer='foo')
        >>> code = '''
        ...
        ... for i in range(5):
        ...     for j in range(5):
        ...         k = i + j
        ...         print(k)
        ...
        ... '''
        >>> tree = ast.parse(code)
        >>> loop, = tree.body

        """
        first = FirstPassFor(self.buffer).visit(copy.deepcopy(loop))
        rest = RestIterableFor(self.buffer).visit(copy.deepcopy(loop))
        return first + [rest]

class ShallowAnnotator(ast.NodeTransformer):
    """Does a shallow annotation on the code given to it

    Literally only do assignment rewrites.

    """
    def __init__(self, buffer):
        super(__class__, self).__init__()
        self.buffer = buffer

    def visit_Assign(self, assign):
        """Append the targets to the assign code string"""
        assign_content, targets_content = astor.to_source(assign), astor.to_source(assign.targets[0])
        content = assign_content + targets_content.strip()
        return make_annotation(
            buffer=self.buffer,
            content=content,
            lineno=assign.lineno if hasattr(assign, 'lineno') else None
        )

    def visit_Expr(self, expr):
        """Don't double-annotate an annotation

        Even in `expr` is a `ast.Call` its `value` might be a `ast.Attribute`
        not a `ast.Name`. In this case we know it's not an annotation. Perhaps
        a more reliable way would be traversing the AST and looking for any
        node with a `id` of `__cell__` or perhaps tagging the node with a
        boolean flag called `is_annotation`.

        Annotations are only *maybe* here at this point because
        `NamespacePromoter` puts them in.

        """
        if isinstance(getattr(expr, 'value', None), ast.Call) and getattr(expr.value.func, 'id', None) == '__cell__':
            return expr
        else:
            return make_annotation(expr, buffer=self.buffer)

    def generic_visit(self, node):
        if isinstance(node, ast.Module):
            return super().generic_visit(node)
        else:
            return make_annotation(node, buffer=self.buffer)


class DeepAnnotator(ShallowAnnotator):
    """Annotates code with commands to create jupyter notebook cells"""

    def _annotate_nodes(self, nodes):
        """Make annotation on the nodes.

        If the node has a namespace then don't annotate it normally.
        Rather recursively call `visit()` on it.

        """
        exprs = []
        for node in nodes:
            new_nodes = self.visit(node)
            if isinstance(new_nodes, list):
                exprs.extend(new_nodes)
            else:
                exprs.append(new_nodes)
        return exprs

    def visit_If(self, iff):
        return [
            make_annotation(buffer=self.buffer, content=f'`if {astor.to_source(iff.test).strip()} ...`', cell_type='2'),
            make_annotation(iff.test, buffer=self.buffer),
            ast.If(
                test=iff.test,
                body=self._annotate_nodes(iff.body),
                orelse=self._annotate_nodes(iff.orelse)
            )
        ]

    def visit_Try(self, try_):
        handlers = []
        for handler in try_.handlers:
            handlers.append(
                ast.ExceptHandler(
                    type=handler.type,
                    name=None,
                    body=self._annotate_nodes(handler.body)
                )
            )
        return ast.Try(
                body=self._annotate_nodes(try_.body),
                handlers=handlers,
                orelse=self._annotate_nodes(try_.orelse),
                finalbody=self._annotate_nodes(try_.finalbody)
        )


    def visit_Assign(self, assign):
        """Append the targets to the assign code string

        Do the same thing as `generic_visit()` otherwise.

        """
        annotated_assign = super().visit_Assign(assign)
        return [assign, annotated_assign]

    def visit_Expr(self, expr):
        annotated_expr = super().visit_Expr(expr)
        if annotated_expr == expr:
            return expr
        else:
            return [annotated_expr, expr]

    def generic_visit(self, node):
        """Catch-all for nodes that slip through

        Basically everything I haven't gotten around to writing a custom
        annotator for gets caught here and wrapped in an annotation. Currently
        the one that jumps to mind are context managers.

        This is necessary because some nodes we recursively call `self.visit()`
        on and we may run into expressions that we have not written a node
        tranformer for.

        """
        if isinstance(node, ast.Module):
            return super().generic_visit(node)
        else:
            return [make_annotation(node, buffer=self.buffer), node]
