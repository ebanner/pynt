from unittest import TestCase

import codebook.node_transformers


class TestSupport(TestCase):
    def test_upcase(self):
        s = 'foo'
        output = codebook.node_transformers.upcase(s)
        expected = 'Foo'
        self.assertEqual(output, expected)
