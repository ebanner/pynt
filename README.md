# PYNT (PYthon iNTeractive)

Emacs minor mode for generating and interacting with jupyter notebooks.

[![MELPA](https://melpa.org/packages/pynt-badge.svg)](https://melpa.org/#/pynt)
[![PyPI version](https://badge.fury.io/py/codebook.svg)](https://badge.fury.io/py/codebook)

[![Built with Spacemacs](https://cdn.rawgit.com/syl20bnr/spacemacs/442d025779da2f62fc86c2082703697714db6514/assets/spacemacs-badge.svg)](http://spacemacs.org)

- [🎥 Feature Tour (YouTube)](https://youtu.be/wtVF5cMhBjg)

![Demo](/img/demo.gif)

## Quick Start

pynt is [available](https://melpa.org/#/pynt) for download through [MELPA](https://melpa.org/). Once you have [configured](https://melpa.org/#/getting-started) emacs to use MELPA then run the following commands in emacs.

```
M-x package-install RET pynt
C-x C-f my-python-file.py
M-x pynt-mode
```

## Feature List

- *On-the-fly notebook creation*
  - Simply start pynt-mode on a python file and a new notebook will be created for you to interact with (provided you have set the variable `pynt-start-jupyter-server-on-startup` to `t`)
- *Dump a region of python code into a jupyter notebook*
  - Selectable regions include functions, methods, and code at the module level (i.e. outside of any function or class)
- *Scroll the resulting jupyter notebook with the code buffer*
  - Alignment between code and cells are preserved even when cells are added and deleted
- *Generate web-browser-based jupyter notebooks*
  - Because pynt generates [EIN](http://millejoh.github.io/emacs-ipython-notebook/) notebooks you can hit `C-x C-s` on the generated notebook which can be [opened up](#jupyter-notebook-web-browser-client) by a jupyter notebook web browser client

## Using pynt

Once you have opened a python file and `pynt-mode` is active you should select the region of code you would like to dump into a jupyter notebook by typing `C-c C-s` and cycling though the resulting code regions. Once you have made a selection hit `C-c C-e` to dump that code region into a jupyter notebook.

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
