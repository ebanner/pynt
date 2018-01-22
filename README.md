# PYNT (PYthon iNTeractive)

Emacs package for generating and interacting with a jupyter notebooks from existing code.

![Demo](/img/demo.gif)

## Feature Tour (YouTube)

[![pynt Demo](http://img.youtube.com/vi/qqJbaoS_sH0/0.jpg)](http://www.youtube.com/watch?v=qqJbaoS_sH0 "pynt Demo")

## What is PYNT?

pynt is fundamentally a python to jupyter notebook (`.py` to `.ipynb`) converter.

It takes as input a python file and produces a jupyter notebook. pynt-mode is an emacs minor mode which provides an interface for making this transformation as well as provides tooling for scrolling the generated notebook.

Another way to think about pynt is it is a sort of "live" log. Imagine if you put print statements between every line of code. The output would be a text file that you could view. pynt outputs not a text file, but a jupyter notebook that you can interact with an execute. Think of it as a "starting point".

Yet another way to think about pynt is it is like a debugger, but rather than only having access to a single line at a time, it is a debugger that is running for every line all at the same time.

## Prerequisites

- Emacs
- jupyter (python)
- EIN [[1]](http://millejoh.github.io/emacs-ipython-notebook/) and [[2]](https://github.com/millejoh/emacs-ipython-notebook)
- epc (python)

## Installation

1. Clone this repo to `/your/local/path/pynt`
2. Add `(load-file "/your/local/path/pynt/code/pynt.el")` to your `.emacs` file

## Using pynt

The first step is starting EIN just like you normally would. My workflow for that is the following.

1. Do `M-x pynt-mode RET` on a python file
2. Pop over to the jupyter notebook and cycle through the namespaces with the `<left>` and `<right>` arrow keys
3. Once you've decided on a namespace pop back over to the code and then hit `C-c C-e` or `M-x pynt-execute-current-namespace RET`
4. Optionally toggle `M-x pynt-scroll-mode RET` to make the generated notebook scroll with your point

## Inspirations and Related Projects

- [SLIME](https://common-lisp.net/project/slime/)
- [vim-slime](https://github.com/jpalardy/vim-slime)
- [EIN](http://millejoh.github.io/emacs-ipython-notebook/)
- [Bret Victor - Inventing on Principle (video)](https://vimeo.com/36579366)
    - [Tools to support live coding as in Bret Victor's “Inventing on Principle” talk](https://stackoverflow.com/questions/9448215/tools-to-support-live-coding-as-in-bret-victors-inventing-on-principle-talk)  (stack overflow)
- [Light Table - a new IDE concept](http://www.chris-granger.com/2012/04/12/light-table-a-new-ide-concept/)
    - [EmacsWiki: Light Table](https://www.emacswiki.org/emacs/LightTable)
    - [Video demonstration and commentary](https://www.youtube.com/watch?v=TgHvRcbYJ-8)
- [CppCon 2014: Mike Acton "Data-Oriented Design and C++"](https://www.youtube.com/watch?v=rX0ItVEVjHc)
- [GOTO 2016 • Pure Functional Programming in Excel • Felienne Hermans](https://www.youtube.com/watch?v=0yKf8TrLUOw)
- [Introduction to Investing and Finance - Lesson 1 by Martin Shkreli](https://www.youtube.com/watch?v=ARrNYyJEnFI&t=1379s)  (expert spreadsheet usage)
- [Bruce Hauman](http://rigsomelight.com/)
    - [Literate interactive coding: Devcards](https://www.youtube.com/watch?v=G7Z_g2fnEDg)
    - [Developing ClojureScript With Figwheel](https://www.youtube.com/watch?v=j-kj2qwJa_E)

## Screenshots

### Jupyter Notebook Web Browser Client

![Browser](/img/browser.png)

### Emacs IPython Notebook Client

![EIN](/img/ein.png)
