# PYNT (PYthon iNTeractive)

Jupyter notebooks for software engineers

- [🎥 Feature Tour (YouTube)](http://www.youtube.com/watch?v=qqJbaoS_sH0 "pynt Demo")
- ![Demo](/img/demo.gif)

## What is pynt?

pynt is fundamentally a python to jupyter notebook (`.py` to `.ipynb`) converter.

pynt takes a python file and produces a jupyter notebook. The resulting jupyter notebook can be opened in a [jupyter notebook web browser client](#jupyter-notebook-web-browser-client) or within emacs with a [Emacs IPython Notebook client](https://github.com/ebanner/pynt/blob/dev/README.md#emacs-ipython-notebook-client).

In addition to this converter an emacs package called `pynt-mode` is provided which provides an interface for issuing this transformation as well as tooling for making the transition from code and jupyter notebook (and vice-versa) seamless.

Another way to think about pynt is as a tool which produces a *dynamic* (or *live*) log. A dynamic log is in contrast to a *static* (or *dead*) log which you could produce by, say, inserting print statements between each line of your code. pynt does this automatically *without you having to insert logging statements*.

Yet another way to think about pynt is as a debugger. But rather than operating at one line at a time (as with traditional debuggers) you have access to *every line of code*.

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
