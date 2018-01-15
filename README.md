# PYNT (PYthon iNTeractive)

Emacs package for generating and interacting with a jupyter notebooks from existing code via [Emacs IPython Notebook](http://millejoh.github.io/emacs-ipython-notebook/).

## Getting Started

These instructions will get you a copy of the project up and running on your local machine.

### Prerequisites

In addition to installing `pynt.el` you must install the `jupyter` and `epc` python packages. You should be able to do a

```shell
pip install --user jupyter epc
```

### Installing

1. Clone this repo to `/your/local/path/pynt`
2. Add `(load-file "/your/local/path/pynt/code/pynt.el")` to your `.emacs` file

*Coming Soon: I am [in the process](https://github.com/melpa/melpa/pull/5240) of making this package available through MELPA!*

## Using pynt

pynt is very much set up currently to augment and streamline your work with [Emacs IPython Notebook](http://millejoh.github.io/emacs-ipython-notebook/). To get the most out of it, I would recommend developing a basic proficiency with that tool.

Once you feel comfortable using EIN, here are some steps to get setup and start using EIN:

1. `M-x ein:jupyter-server-start RET`
2. Create a new notebook by moving your point over to **[New Notebook]** and hitting `RET`
3. Do `M-x pynt-mode RET` in your code window
4. Click on the namespace you are interested in interacting with with either the mouse or with `ein:notebook-worksheet-open-1th`
5. Hit `C-c C-e` to do `pynt-execute-namespace` to generate a jupyter notebook
6. Optionally run `M-x pynt-scroll-mode RET` to enable scrolling mode to line up your code with the generated notebook

## Video Demo

[![pynt Demo](http://img.youtube.com/vi/OkdkJ2fu_Oc/0.jpg)](http://www.youtube.com/watch?v=OkdkJ2fu_Oc "pynt Demo")


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
