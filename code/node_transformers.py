"""Node transformers for manipulating the abstract syntax tree

Transformers for exploding functions, rewriting syntax, and adding annotations
exist currently.

"""

import ast
import doctest
import random
import string

import astor


def __random_string__():
    return ''.join(random.choice(string.ascii_lowercase) for _ in range(10))


class FunctionExploder(ast.NodeTransformer):
    """Exposes the body of a function to the next scope up

    Transforms:

        def foo(a, b, c):
            '''Consise description

            Longer description is included under the concise description

            >>> a = 1
            >>> b = 2
            >>> c = 3

            '''
            z = a + b + c
            return z

    into something like

        m: # Foo

        m: Consise description

        m: Longer description is included under the concise description

        c: a = 1
        c: b = 2
        c: c = 3
        c: z = a + b + c

    TODO: Handle `return`s better.

    """

    def visit_FunctionDef(self, func):
        """Roll out a function definition

        >>> self = FunctionExploder()
        >>> code = '''
        ...
        ... x
        ... def foo(a):
        ...     \"\"\"An example function
        ...
        ...     A detailed description.
        ...
        ...     >>> a = 5
        ...
        ...     \"\"\"
        ...     b = a + 1
        ...     return b
        ... y
        ... '''
        >>> tree = ast.parse(code)
        >>> func = tree.body[1]

        """
        [docstring], func.body = func.body[:1], func.body[1:]

        # docstring
        parser = doctest.DocTestParser()
        results = parser.parse(docstring.value.s)
        docstring_prefix, docstring_examples = results[0], [result for result in results if isinstance(result, doctest.Example)]
        assign_exprs = [example.source.strip() for example in docstring_examples]

        # filter returns
        func.body = [stmt for stmt in func.body if not isinstance(stmt, ast.Return)]

        # augment body with docstring
        exprs = []
        exprs.append(
            Annotator.make_annotation(
                buffer=func.name,
                content=' '.join(substring.capitalize() for substring in func.name.split('_')),
                cell_type='1',
                lineno=func.lineno
            )
        )
        exprs.append(Annotator.make_annotation(buffer=func.name, content=docstring_prefix, cell_type='markdown'))
        exprs.append(Annotator.make_annotation(buffer=func.name, content='Example Input', cell_type='1'))
        for assign_expr in assign_exprs:
            tree = ast.parse(assign_expr)
            exprs.append(tree.body[0])
        exprs.append(Annotator.make_annotation(buffer=func.name, content='Body of Function', cell_type='1'))
        for stmt in func.body:
            exprs.append(stmt)

        return exprs

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

        for i in iterable:
            <body>

        becomes

        p = iter(iterable)
        while True:
            try:
                i = next(p)
            except StopIteration:
                break
            <body>

        >>> self = SyntaxRewriter(buffer='foo')
        >>> code = '''
        ...
        ... x
        ... for i in range(2):
        ...     for j in range(2):
        ...         k = i + j
        ...         print(k)
        ... y
        ...
        ... '''
        >>> tree = ast.parse(code)
        >>> loop = tree.body[1]

        """
        loop = self.generic_visit(loop)

        # p = iter(iterable)
        var = __random_string__()
        assign_iter = ast.Assign(
            targets=[ast.Name(id=var, ctx=ast.Store())],
            value=ast.Call(
                func=ast.Name(id='iter', ctx=ast.Load()),
                args=[loop.iter],
                keywords=[]
            )
        )

        # i = next(iter(iterable))
        assign_next = ast.Assign(
            targets=[loop.target],
            value=ast.Call(
                func=ast.Name(id='next', ctx=ast.Load()),
                args=[ast.Name(id=var, ctx=ast.Load())],
                keywords=[]
            )
        )

        # try:
        #     p = iter(iterable)
        # except:
        #     break
        try_node = ast.Try(
            body=[assign_next],
            handlers=[ast.ExceptHandler(type=ast.Name(id='StopIteration', ctx=ast.Load()), name=None, body=[ast.Break()])],
            orelse=[],
            finalbody=[]
        )

        # while True:
        #     try:
        #         p = iter(iterable)
        #     except:
        #        break
        while_node = ast.While(
            test=ast.NameConstant(value=True),
            body=[try_node] + loop.body,
            orelse=[]
        )

        content = f'`for {astor.to_source(loop.target).strip()} in {astor.to_source(loop.iter).strip()} ...`'
        exprs = [
            Annotator.make_annotation(buffer=self.buffer, content=content, cell_type='2', lineno=loop.lineno),
            assign_iter,
            while_node
        ]
        return exprs

class Annotator(ast.NodeTransformer):
    """Annotates code with commands to create jupyter notebook cells"""

    @staticmethod
    def make_annotation(node=None, buffer='outside', content=None, cell_type='code', lineno=None):
        """Return a ast.Expr that looks like

        __cell__('make-code-cell-and-eval', [content, buffer, cell_type])

        """
        content = astor.to_source(node).strip() if node else content
        lineno = str(node.lineno) if hasattr(node, 'lineno') else str(-1) if not lineno else str(lineno)
        call = ast.Call(
            func=ast.Name(id='__cell__', ctx=ast.Load()),
            args=[
                ast.Str(s=content),
                ast.Str(s=f'context={buffer}'),
                ast.Str(s=cell_type),
                ast.Str(s=lineno),
            ],
            keywords=[]
        )
        return ast.Expr(call)

    def __init__(self, buffer):
        super(__class__, self).__init__()
        self.buffer = buffer

    def _annotate_nodes(self, nodes):
        """Make annotation on the nodes.

        If the node has a context then don't annotate it normally.
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
            Annotator.make_annotation(buffer=self.buffer, content=f'`if {astor.to_source(iff.test).strip()} ...`', cell_type='2'),
            Annotator.make_annotation(iff.test, buffer=self.buffer),
            ast.If(
                test=iff.test,
                body=self._annotate_nodes(iff.body),
                orelse=self._annotate_nodes(iff.orelse)
            )
        ]

    def visit_While(self, whilst):
        return [
            Annotator.make_annotation(buffer=self.buffer, content=f'`while {astor.to_source(whilst.test).strip()} ...`', cell_type='2'),
            Annotator.make_annotation(whilst.test, buffer=self.buffer),
            ast.While(
                test=whilst.test,
                body=self._annotate_nodes(whilst.body),
                orelse=self._annotate_nodes(whilst.orelse),
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
        assign_content, targets_content = astor.to_source(assign), astor.to_source(assign.targets[0])
        content = assign_content + targets_content.strip()
        target = astor.to_source(assign.targets[0]).strip()
        return [
            Annotator.make_annotation(buffer=self.buffer, content=f'`{target} = ...`', cell_type='2'),
            assign,
            Annotator.make_annotation(buffer=self.buffer, content=content, lineno=assign.lineno if hasattr(assign, 'lineno') else None),
        ]

    def visit_Expr(self, expr):
        """Don't double-annotate an annotation

        Even in `expr` is a `ast.Call` its `value` might be a `ast.Attribute`
        not a `ast.Name`. In this case we know it's not an annotation. Perhaps
        a more reliable way would be traversing the AST and looking for any
        node with a `id` of `__cell__` or perhaps tagging the node with a
        boolean flag called `is_annotation`.

        Annotations are only *maybe* here at this point because
        `FunctionExploder` puts them in.

        """
        if isinstance(expr.value, ast.Call) and getattr(expr.value.func, 'id', None) == '__cell__':
            return expr
        else:
            return [expr, Annotator.make_annotation(expr, buffer=self.buffer)]

    def generic_visit(self, node):
        """Catch-all for nodes that slip through

        Basically everything I haven't gotten around to writing a custom
        annotator for gets caught here and wrapped in an annotation. Currently
        the one that jumps to mind are context managers.

        """
        if isinstance(node, ast.Module):
            return super().generic_visit(node)
        else:
            return [node, Annotator.make_annotation(node, buffer=self.buffer)]


if __name__ == '__main__':
    code = '''

    def foo(a):
        """This is a docstring

        >>> a = 7

        """
        for i in range(a):
            print(i)

    '''
    tree = ast.parse(code)
    tree = FunctionExploder().visit(tree)
    code = astor.to_source(tree)
    print(code)

    tree = SyntaxRewriter(buffer='foo').visit(tree)
    code = astor.to_source(tree)
    print(code)

    tree = Annotator(buffer='foo').visit(tree)
    code = astor.to_source(tree)
    print(code)

    code = '''

    if foo in bar:
        width, height = scene_image.size
        for i, obj in enumerate(mod_vec_payload['objects']):
            print(1)
            print(2)

    # Cropping and processing the object patches from the scene image
    object_arrays, object_imgs = [], []
    for i, obj in tqdm(enumerate(mod_vec_payload['objects'])):
        print(3)
        print(4)

    with graph.as_default():
        eprint('GOT TF GRAPH AND VECTORIZING')
        all_object_vectors = predictF2V(xception_ftr_xtrct, object_arrays)

    '''
    tree = ast.parse(code)
    tree = FunctionExploder().visit(tree)
    code = astor.to_source(tree)
    print(code)
    tree = SyntaxRewriter(buffer='outside').visit(tree)
    code = astor.to_source(tree)
    print(code)
    tree = Annotator(buffer='outside').visit(tree)
    code = astor.to_source(tree)
    print(code)

    code = '''

    for i in range(2):
        print(i)

    '''
    tree = ast.parse(code)
    tree = FunctionExploder().visit(tree)
    code = astor.to_source(tree)
    print(code)
    tree = ast.parse(code)
    tree.body = [SyntaxRewriter(buffer='outside').visit(node) for node in tree.body]
    code = astor.to_source(tree)
    print(code)
    tree = ast.parse(code)
    tree.body = [Annotator(buffer='outside').visit(node) for node in tree.body]
    code = astor.to_source(tree)
    print(code)
