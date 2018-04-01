# PYNT (PYthon iNTeractive)

Get your code into a jupyter notebook. Anytime. Anywhere.

[![MELPA](https://melpa.org/packages/pynt-badge.svg)](https://melpa.org/#/pynt) [![PyPI version](https://badge.fury.io/py/codebook.svg)](https://badge.fury.io/py/codebook) [![Built with Spacemacs](https://cdn.rawgit.com/syl20bnr/spacemacs/442d025779da2f62fc86c2082703697714db6514/assets/spacemacs-badge.svg)](http://spacemacs.org)

## Quick Start

*Disclaimer: pynt is in beta. Make sure to back-up your code before using it!*

First install the codebook module [from PyPI](https://pypi.python.org/pypi/codebook) with [pip](https://pip.pypa.io/en/stable/).

```
pip install codebook
```

Then open emacs and download and install pynt through [MELPA](https://melpa.org/#/pynt).

```
M-x package-install RET pynt
```

Finally open some source code and activate pynt mode.

```
C-x C-f my-python-file.py
M-x pynt-mode
```

## Selected Features

### On-the-fly notebook creation

No more copy and pasting code into jupyter notebooks. Expressions are automatically inserted into their own cells.

![Alt Text](https://github.com/ebanner/pynt-assets/blob/master/gif/generate-notebook.gif)

### Attach a jupyter notebook to a running process

Run a command which hits the code in the notebook. Restart the notebook kernel to attach to that process.

![Alt Text](https://github.com/ebanner/pynt-assets/blob/master/gif/attach%20notebook.gif)
  
### Syntax transformations

Unroll the first pass of loops for increased interactivity.
  
![Alt Text](https://github.com/ebanner/pynt-assets/blob/master/gif/loop%20unrolling.gif)

### Scroll the resulting jupyter notebook with the code buffer

Never forget which cell a code line corresponds to.

![Alt Text](https://github.com/ebanner/pynt-assets/blob/master/gif/scroll-notebook.gif)

## What is pynt?

pynt is an emacs minor mode for getting regions of code (e.g. function and methods) into jupyter notebooks. If you have access to the source and a command to call it with then you can get your code into a jupyter notebook.

pynt strives to make it possible to promote all regions of code into the global namespace. Examples include functions, methods, and loops. Once your code is in the global namespace it becomes possible to interact with it.

## Using pynt

It is highly recommended that you familiarize yourself with [Emacs IPython Notebook (EIN)](http://millejoh.github.io/emacs-ipython-notebook/) first as pynt at its core is a tool to make working with EIN easier.

Once you have opened a python file and pynt mode is active, cursor over to the region of code you would like to dump into a notebook and hit `C-c C-s`.

If you want to attach a jupyter notebook to a running process, then run a command which hits the jupyter notebook code. Restart the jupyter notebook kernel with `C-c C-r` (`ein:notebook-restart-kernel-command`). When you see the message `ein: [info] Starting channels WS: ...` your notebook is attached!

## How it works

pynt uses a [custom kernel manager](https://github.com/ebanner/extipy) for attaching to jupyter notebook kernels started via third-party processes. When pynt generates a jupyter notebook from a code region that code region is replaced with a IPython kernel breakpoint so that subsequent commands that hit it will start a jupyter kernel for the notebook to attach to.

## Related Projects

pynt is a tool that truly [stands on the shoulders of giants](https://en.wikipedia.org/wiki/Standing_on_the_shoulders_of_giants). Here are some projects where if they had not existed, then pynt would not have been possible.

- [Jupyter](http://jupyter.org/)
  - [Emacs IPython Notebook](http://millejoh.github.io/emacs-ipython-notebook/)
- [Emacs](https://www.gnu.org/software/emacs/)
  - [Spacemacs](http://spacemacs.org/)
- [Python](https://www.python.org/)
- [SLIME](https://common-lisp.net/project/slime/)
  - [vim-slime](https://github.com/jpalardy/vim-slime)
  
