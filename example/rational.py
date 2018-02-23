"""

Class for rational numbers.

A rational number is the ratio of two numbers. Examples of rational numbers
include...

- 3/2
- 1/1
- 4/6

Rational numbers need not be normalized.

"""

class RationalNumber:
    def __init__(self, n, d):
        """Constructor

        >>> self = RationalNumber.__new__(RationalNumber)
        >>> n = 3
        >>> d = 2

        """
        self.n = n
        self.d = d
