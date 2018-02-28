from setuptools import setup

setup(
    name='codebook',
    version='1.0.0',
    description='Emacs minor mode for generating and interacting with jupyter notebooks',
    url='http://github.com/ebanner/pynt',
    author='Edward Banner',
    author_email='edward.banner@gmail.com',
    license='MIT',
    packages=['codebook'],
    zip_safe=False,
    scripts=['bin/embed-kernel']
)
