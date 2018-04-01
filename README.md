# PYNT (PYthon iNTeractive)

Emacs minor mode for generating and interacting with jupyter notebooks.

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

No more copy and pasting from code into jupyter notebooks. Expressions are split up into their own cells.

![Alt Text](https://github.com/ebanner/pynt-assets/blob/master/gif/generate-notebook.gif)

### Attach a jupyter notebook to a running process

Run a command which hits the code region. Restart the notebook kernel to attach to that process.

![Alt Text](https://github.com/ebanner/pynt-assets/blob/master/gif/attach%20notebook.gif)
  
### Syntax transformations

Unroll the first pass of loops for increased interactivity.
  
![Alt Text](https://github.com/ebanner/pynt-assets/blob/master/gif/loop%20unrolling.gif)

### Scroll the resulting jupyter notebook with the code buffer

Never forget which cell a code line corresponds to.

![Alt Text](https://github.com/ebanner/pynt-assets/blob/master/gif/scroll-notebook.gif)

## Using pynt

Once you have opened a python file and pynt mode is active you should select the region of code you would like to dump into a jupyter notebook by typing `C-c C-s` and cycling though the resulting code regions. Once you have made a selection hit `C-c C-e` to dump that code region into a jupyter notebook.

It is recommended at this point to enable `pynt-scroll-mode` which scrolls (i.e. aligns) the notebook cells with the code lines. You can activate `pynt-scroll-mode` with `M-x pynt-scroll-mode RET`.

## EIN Tweaks

There are a few tweaks to EIN that make the pynt/EIN experience nicer IMO.

- Have `C-<return>` and `S-<return>` behave [as they would in a jupyter web browser client](https://github.com/ebanner/dotfiles/blob/deed94b024612ca1ed9c1e98f8e98ade793208a2/spacemacs#L473-L476)
- [Arrow through the worksheets](https://github.com/ebanner/dotfiles/blob/deed94b024612ca1ed9c1e98f8e98ade793208a2/spacemacs#L479-L480) in a EIN notebook buffer (warning: [evil](https://github.com/emacs-evil/evil)-specific)
- Prevent jupyter from [popping open a new tab](https://github.com/ebanner/dotfiles/blob/deed94b024612ca1ed9c1e98f8e98ade793208a2/spacemacs#L496) in your web browser on startup


## Related Projects

pynt is a tool that truly [stands on the shoulders of giants](https://en.wikipedia.org/wiki/Standing_on_the_shoulders_of_giants). Here are some projects where if they had not existed, then pynt would not have been possible.

- [Jupyter](http://jupyter.org/)
  - [Emacs IPython Notebook](http://millejoh.github.io/emacs-ipython-notebook/)
- [Emacs](https://www.gnu.org/software/emacs/)
  - [Spacemacs](http://spacemacs.org/)
- [Python](https://www.python.org/)
- [SLIME](https://common-lisp.net/project/slime/)
  - [vim-slime](https://github.com/jpalardy/vim-slime)

## Screenshots

### Jupyter Notebook Web Browser Client

![Browser](/img/browser.png)

### Emacs IPython Notebook Client

![EIN](/img/ein.png)
