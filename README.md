# PYNT (PYthon iNTeractive)

Emacs minor mode for generating and interacting with jupyter notebooks.

[![Built with Spacemacs](https://cdn.rawgit.com/syl20bnr/spacemacs/442d025779da2f62fc86c2082703697714db6514/assets/spacemacs-badge.svg)](http://spacemacs.org)

- [🎥 Feature Tour (YouTube)](http://www.youtube.com/watch?v=qqJbaoS_sH0 "pynt Demo")

![Demo](/img/demo.gif)

## Quick Start

pynt is [available](https://melpa.org/#/pynt) for download through [MELPA](https://melpa.org/). Once you have [configured](https://melpa.org/#/getting-started) emacs to use MELPA then run the following commands in emacs.

```
M-x package-install RET pynt
C-x C-f my-python-file.py
M-x pynt-mode
```

## Feature List

- *Dump a region of python code into a jupyter notebook*
  - Selectable regions include functions, methods, and code at the module level (i.e. outside of any function or class)
- *Scroll the resulting jupyter notebook with the code buffer*
  - Alignment between code and cells are preserved even when cells are added and deleted
- *Generate web-browser-based jupyter notebooks*
  - Because pynt generates [EIN](http://millejoh.github.io/emacs-ipython-notebook/) notebooks you can hit `C-x C-s` on the generated notebook to save it as a `.ipynb` file which can be [opened up](#jupyter-notebook-web-browser-client) by a jupyter notebook web browser client

## Using pynt

Once you have opened a python file and `pynt-mode` is active you should select the region of code you would like to dump into a jupyter notebook by typing `C-c C-s` and cycling though the resulting code regions. Once you have made a selection hit `C-c C-e` to dump that code region into a jupyter notebook.

It is recommended at this point to enable `pynt-scroll-mode` which scrolls (i.e. aligns) the notebook cells with the code lines. You can activate `pynt-scroll-mode` with `M-x pynt-scroll-mode RET`.

## Inspirations and Related Projects

There are many projects, talks, and one-off instances of interactive programming "in the wild" that inspired me to create pynt. Here are a few of my favorites.

- [SLIME](https://common-lisp.net/project/slime/)
  - [vim-slime](https://github.com/jpalardy/vim-slime)
- [Jupyter](http://jupyter.org/)
  - [Emacs IPython Notebook](http://millejoh.github.io/emacs-ipython-notebook/)
- [Introduction to Investing and Finance by Martin Shkreli](https://www.youtube.com/watch?v=ARrNYyJEnFI&t=1379s)  (expert spreadsheet usage)
- [Pure Functional Programming in Excel • Felienne Hermans](https://www.youtube.com/watch?v=0yKf8TrLUOw)
- [Light Table - a new IDE concept](http://www.chris-granger.com/2012/04/12/light-table-a-new-ide-concept/)
    - [EmacsWiki: Light Table](https://www.emacswiki.org/emacs/LightTable)
    - [Video demonstration and commentary](https://www.youtube.com/watch?v=TgHvRcbYJ-8)
- [CppCon 2014: Mike Acton "Data-Oriented Design and C++"](https://www.youtube.com/watch?v=rX0ItVEVjHc)
- [Bruce Hauman](http://rigsomelight.com/)
    - [Literate interactive coding: Devcards](https://www.youtube.com/watch?v=G7Z_g2fnEDg)
    - [Developing ClojureScript With Figwheel](https://www.youtube.com/watch?v=j-kj2qwJa_E)
- [Bret Victor - Inventing on Principle (video)](https://vimeo.com/36579366)
    - [Tools to support live coding as in Bret Victor's “Inventing on Principle” talk](https://stackoverflow.com/questions/9448215/tools-to-support-live-coding-as-in-bret-victors-inventing-on-principle-talk)  (stack overflow)
- [Debugger Driven Developement with Pry by Joel Turnbull](https://www.youtube.com/watch?v=4hfMUP5iTq8)

## Screenshots

### Jupyter Notebook Web Browser Client

![Browser](/img/browser.png)

### Emacs IPython Notebook Client

![EIN](/img/ein.png)
