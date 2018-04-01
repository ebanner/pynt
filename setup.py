import pypandoc

from setuptools import setup


def readme():
    rst = pypandoc.convert_file('README.md', to='rst')
    return rst

setup(
    name='codebook',
    version='1.2.1',
    description='Emacs minor mode for generating EIN notebooks',
    long_description=readme(),
    url='http://github.com/ebanner/pynt',
    classifiers=[
        'Development Status :: 3 - Alpha',
        'Environment :: MacOS X',
        'Programming Language :: Python :: 3.6',
        'Framework :: IPython',
        'Framework :: Jupyter',
        'Intended Audience :: Developers',
        'Intended Audience :: Developers',
        'Intended Audience :: Education',
        'License :: OSI Approved :: MIT License',
        'Natural Language :: English',
        'Operating System :: MacOS',
        'Topic :: Education',
        'Topic :: Software Development',
        'Topic :: Software Development :: Code Generators',
        'Topic :: Software Development :: Debuggers',
        'Topic :: Software Development :: Documentation',
        'Topic :: Text Editors',
        'Topic :: Text Editors :: Emacs',
        'Topic :: Text Editors :: Integrated Development Environments (IDE)'
    ],
    keywords='interactive programming jupyter ipython emacs',
    author='Edward Banner',
    author_email='edward.banner@gmail.com',
    license='MIT',
    packages=['codebook'],
    scripts=[
        'bin/pynt-embed',
        'bin/pynt-serve',
    ],
    install_requires=[
        'jupyter',
        'astor',
        'plac',
        'epc'
    ],
    test_suite='nose.collector',
    tests_require=['nose'],
    include_package_data=True,
    zip_safe=False,
)
