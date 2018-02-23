"""Embed a IPython kernel into a function

Script for embedding a IPython kernel into a function so we can open a jupyter
notebook and talk to it.

"""

import ast
import shutil
import subprocess
import tempfile

import astor
import plac
from node_transformers import IPythonEmbedder


@plac.annotations(
        fname=('file name to embed kernel in', 'option', None, str),
        namespace=('namespace to embed kernel in', 'option', None, str),
        cmd=('namespace to embed kernel in', 'option', None, str),
)
def main(fname='biz.py', namespace='biz.bar', cmd='python biz.py'):
    """Embed a kernel in `fname` in `namespace`

    Args:
        fname (str): file name to embed kernel in
        namespace (str): namespace to embed kernel in
        cmd (str): command which invokes `fname`

    Returns:
        None

    You need to pass `fname` so we know which file to modify/make a temporary
    copy of. You need to pass `namespace` so we know which function to embed.
    You need to pass `cmd` so we know which command to invoke so that
    `namespace` gets called.

    Is anything redundant?

    """
    # Read in Input File
    with open(fname) as f:
        lines = f.readlines()
    code = ''.join(lines)

    # Embed an IPython Kernel into the Function
    tree = ast.parse(code)
    t = IPythonEmbedder(namespace).visit(tree)
    code_ = astor.to_source(t)

    # Copy the Old File to a Temporary File
    t = tempfile.NamedTemporaryFile(delete=False)
    shutil.copyfile(fname, t.name)

    # Dump New Code to `fname`
    with open(fname, 'w') as f:
        f.write(code_)

    # Run `cmd`
    p = subprocess.run(cmd.split())
    assert p.returncode == 0

    # Move Original File Back
    shutil.copyfile(t.name, fname)


if __name__ == '__main__':
    plac.call(main)
