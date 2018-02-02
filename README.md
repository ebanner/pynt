# PYNT (PYthon iNTeractive)

Jupyter notebooks for software engineers.

[![Built with Spacemacs](https://cdn.rawgit.com/syl20bnr/spacemacs/442d025779da2f62fc86c2082703697714db6514/assets/spacemacs-badge.svg)](http://spacemacs.org)

- [🎥 Feature Tour (YouTube)](http://www.youtube.com/watch?v=qqJbaoS_sH0 "pynt Demo")

![Demo](/img/demo.gif)

## Quick Start

```shell
git clone https://github.com/ebanner/pynt.git
echo '(load-file "~/pynt/pynt/pynt.el")' >> .emacs
pip install --user jupyter epc
emacs my-python-file.py
M-x pynt-mode RET
```

## What is pynt?

pynt is fundamentally a python to jupyter notebook (`.py` to `.ipynb`) converter.

pynt takes a python file and produces a jupyter notebook. The resulting jupyter notebook can be opened in a [jupyter notebook web browser client](#jupyter-notebook-web-browser-client) or in emacs with a [Emacs IPython Notebook client](https://github.com/ebanner/pynt/blob/dev/README.md#emacs-ipython-notebook-client).

In addition an emacs package called `pynt-mode` is included. `pynt-mode` provides an interface for invoking pynt as well as tooling for making the transition from code and jupyter notebook (and vice-versa) seamless (i.e. by scrolling jupyter cells with lines of code).

One way to think about pynt is it is a tool which produces a *dynamic* log which is *alive*. A dynamic log is in contrast to a *static* log which is *dead*. Examples of *dead* logs are text files, markdown files, or PDFs. For example, one could produce a dead log by inserting print statements between each line of your code, generating a text file. However, this would require you to modify your existing code. pynt produces a live log *without you having to modify the existing code*.

Yet another way to think about pynt is as a *multi-line debugger*. Rather than having access to a single line at a time (as with traditional debuggers) pynt gives you access to *every line of code at all times* via a jupyter notebook.

## What are pynt use cases?

pynt shines in the following scenarios:

1. Acquainting/familiarizing yourself with someone else's code
2. Modifying your code
3. Producing documentation to familiarize someone else with your code

Regarding becoming familiar with someone else's code, a common workflow of mine is to paste the foreign code into a jupyter notebook, split up each line into its own jupyter cell (along with `print` statements), then evaluate each cell to see the effect of each line of code. **This is exactly what pynt does**.

Regarding modifying your code, pynt provides an alternative to the edit-compile-run cycle by allowing you to run your local (i.e. single-line) change in isolation and *see* its local effect immediately. Typically this would be inferred by running the surrounding function and inspecting the output (or adding print statements).

Regarding producing documentation, since pynt mixes markdown with code and data, you can hand this document off to someone  else so they can quickly get up to speed with how your code works.

## Using pynt

Once you have opened a python file and `pynt-mode` is active you should select the region of code you would like to dump into a jupyter notebook (by popping over to the notebook buffer and cycling through the available namespaces with the `<left>` and `<right>` arrow keys) then hitting `C-c C-e`.

It is recommended at this point to enable `pynt-scroll-mode` which scrolls (i.e. aligns) the notebook buffer cells with the code lines. You can activate `pynt-scroll-mode` with `M-x pynt-scroll-mode RET`.

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
