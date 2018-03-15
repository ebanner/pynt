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

class TestSupport(TestCase):
    def test_upcase(self):
        s = 'foo'
        output = codebook.node_transformers.upcase(s)
        expected = 'Foo'
        self.assertEqual(output, expected)
