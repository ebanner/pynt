import ast
from unittest import TestCase

import astor

import codebook.node_transformers


class TestFirstPassForSimple(TestCase):
    def test_iter(self):
        code = """

for i in range(5):
    print(i)

"""
        tree = ast.parse(code)
        m = codebook.node_transformers.FirstPassForSimple(buffer='foo').visit(tree)
        out = astor.to_source(m)
        self.assertIn('iter', out)

    def test_nested(self):
        code = """

for i in range(5):
    for j in range(5):
        k = i + j
        print(k)

"""
        tree = ast.parse(code)
        m = codebook.node_transformers.FirstPassForSimple(buffer='foo').visit(tree)
        out = astor.to_source(m)
        self.assertIn('iter', out)

    def test_nobreak(self):
        code = """

for i in range(5):
    print(i)
    break

"""
        tree = ast.parse(code)
        m = codebook.node_transformers.FirstPassForSimple(buffer='foo').visit(tree)
        out = astor.to_source(m)
        self.assertNotIn('break', out)
        self.assertIn('pass', out)

class TestSupport(TestCase):
    def test_upcase(self):
        s = 'foo'
        output = codebook.node_transformers.upcase(s)
        expected = 'Foo'
        self.assertEqual(output, expected)

class TestFilters(TestCase):
    def test_filter_class(self):
        code = """

class Foo:
    def bar():
        pass

class Bar:
    def biz():
        pass

"""
        tree = ast.parse(code)
        out = codebook.node_transformers.ClassFinder(class_name='Foo').visit(tree)
        c = astor.to_source(out)
        self.assertIn('Foo', c)
        self.assertNotIn('Bar', c)

    def test_filter_function(self):
        code = """

def foo():
    pass

def bar():
    pass

"""
        tree = ast.parse(code)
        out = codebook.node_transformers.FunctionFinder(func_name='foo').visit(tree)
        c = astor.to_source(out)
        self.assertIn('foo', c)
        self.assertNotIn('bar', c)

class TestShallowAnnotator(TestCase):
    def test_simple(self):
        code = """

a = 3
b = a + 2

"""
        module = ast.parse(code)
        out = codebook.node_transformers.SimpleAnnotator(buffer='foo').visit(module)
        c = astor.to_source(out)
        self.assertIn('__cell__', c)

class TestExpressionFinder(TestCase):
    def test_simple(self):
        code = """

a
b
c

"""
        module = ast.parse(code)
        out = codebook.node_transformers.ExpressionFinder(lineno=4).visit(module)
        self.assertIsInstance(out, ast.Module)
        c = astor.to_source(out)
        self.assertIn('b', c)
        self.assertNotIn('a', c)
        self.assertNotIn('c', c)
