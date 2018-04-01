"""

Class for rational numbers.

A rational number is the ratio of two numbers. Examples of rational numbers
include...

- 3/2
- 1/1
- 4/6

Rational numbers need not be normalized.

"""

import fractions


class RationalNumber:
    def __init__(self, n, d):
        """Constructor

        >>> self = RationalNumber.__new__(RationalNumber)
        >>> n = 4
        >>> d = 2

        """
        self.n = n
        self.d = d

    def __repr__(self):
        """Return a string representation

        >>> self = RationalNumber(n=4, d=2)

        """
        r = f'{self.n}/{self.d}'
        return r

    def _normalize(self):
        """Return the fraction in simplied form

        >>> self = RationalNumber(n=4, d=2)

        """
        gcd = fractions.gcd(self.n, self.d)
        n_ = int(self.n / gcd)
        d_ = int(self.d / gcd)
        r_ = RationalNumber(n_, d_)
        return r_

