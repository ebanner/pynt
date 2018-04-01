from unittest import TestCase

import codebook.syntax


class TestAnnotate(TestCase):
    def test_module(self):
        code = """
x
def bar():
    pass
class Biz:
    def baz(self):
        pass
def qux():
    pass
y"""
        namespace = 'foo'
        out = codebook.syntax.annotate(code, namespace)
        self.assertIn('x', out)
        self.assertIn('y', out)
        self.assertNotIn('bar', out)
        self.assertNotIn('Biz', out)
        self.assertNotIn('qux', out)

    def test_func(self):
        code = """
x
def bar():
    pass
class Biz:
    def baz(self):
        pass
def qux():
    pass
y"""
        namespace = 'foo.bar'
        out = codebook.syntax.annotate(code, namespace)
        self.assertIn('foo.bar', out)
        self.assertNotIn('Biz', out)
        self.assertNotIn('qux', out)
        self.assertNotIn('baz', out)
