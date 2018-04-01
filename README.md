# PYNT (PYthon iNTeractive)

Emacs minor mode for generating and interacting with [Emacs IPython](http://millejoh.github.io/emacs-ipython-notebook/) (i.e. jupyter) notebooks.

[![MELPA](https://melpa.org/packages/pynt-badge.svg)](https://melpa.org/#/pynt) [![PyPI version](https://badge.fury.io/py/codebook.svg)](https://badge.fury.io/py/codebook) [![Built with Spacemacs](https://cdn.rawgit.com/syl20bnr/spacemacs/442d025779da2f62fc86c2082703697714db6514/assets/spacemacs-badge.svg)](http://spacemacs.org)

## Quick Start

First install the codebook module [from PyPI](https://pypi.python.org/pypi/codebook) with [pip](https://pip.pypa.io/en/stable/).

```
pip install codebook
```

pynt is [available](https://melpa.org/#/pynt) for download through [MELPA](https://melpa.org/). Once you have [configured](https://melpa.org/#/getting-started) emacs to use MELPA then run the following commands in emacs.

```
M-x package-install RET pynt
C-x C-f my-python-file.py
M-x pynt-mode
```

## Selected Features

### On-the-fly notebook creation

No more copy and pasting from code into jupyter notebooks. Expressions are automatically inserted into their own cells.

![Alt Text](https://github.com/ebanner/pynt-assets/blob/master/gif/generate-notebook.gif)

### Attach a jupyter notebook to a running process

Run a command which hits the code in the jupyter notebook. Restart the notebook kernel to attach to that process.

![Alt Text](https://github.com/ebanner/pynt-assets/blob/master/gif/attach%20notebook.gif)
  
### Syntax transformations

Unroll the first pass of loops for increased interactivity.
  
![Alt Text](https://github.com/ebanner/pynt-assets/blob/master/gif/loop%20unrolling.gif)

### Scroll the resulting jupyter notebook with the code buffer

Never forget which cell a code line corresponds to.

![Alt Text](https://github.com/ebanner/pynt-assets/blob/master/gif/scroll-notebook.gif)

## Using pynt

It is highly recommended that you familiarize yourself with [Emacs IPython Notebook (EIN)](http://millejoh.github.io/emacs-ipython-notebook/) first as pynt at its core is a tool to make working with EIN easier.

Once you have opened a python file and pynt mode is active, cursor over to the region of code you would like to dump into a notebook and hit `C-c C-s`.

If you want to attach a jupyter notebook to a running process, then run a command which hits the jupyter notebook code. Restart the jupyter notebook with `C-c C-r` (`ein:notebook-restart-kernel-command`). When you see the message `ein: [info] Starting channels WS: ...` your notebook is attached!

## Related Projects

pynt is a tool that truly [stands on the shoulders of giants](https://en.wikipedia.org/wiki/Standing_on_the_shoulders_of_giants). Here are some projects where if they had not existed, then pynt would not have been possible.

- [Jupyter](http://jupyter.org/)
  - [Emacs IPython Notebook](http://millejoh.github.io/emacs-ipython-notebook/)
- [Emacs](https://www.gnu.org/software/emacs/)
  - [Spacemacs](http://spacemacs.org/)
- [Python](https://www.python.org/)
- [SLIME](https://common-lisp.net/project/slime/)
  - [vim-slime](https://github.com/jpalardy/vim-slime)
  
