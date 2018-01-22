# PYNT (PYthon iNTeractive)

Emacs package for generating and interacting with a jupyter notebooks from existing code.

## Screenshots

The jupyter notebook that pynt generates can be viewed with a jupyter notebook web browser client of with a Emacs IPython Notebook client.

### Jupyter Notebook Web Browser Client

![Browser](/img/browser.png)

### Emacs IPython Notebook Client

![EIN](/img/ein.png)

## Demonstration

[![pynt Demo](http://img.youtube.com/vi/OkdkJ2fu_Oc/0.jpg)](http://www.youtube.com/watch?v=OkdkJ2fu_Oc "pynt Demo")

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

1. `M-x ein:notebooklist-login RET 8888 RET my-secret-password RET`
2. `M-x ein:notebooklist-open RET 8888`
3. Create a new notebook or choose an existing one (e.g. `*ein: http://127.0.0.1:8888/Untitled.ipynb*`
4. Pull up your code side-by-side with the worksheet
5. `M-x pynt-mode RET` to activate pynt-mode and select the notebook you chose in step 3
6. `M-x pynt-generate-worksheet RET` to create blank worksheets for each function and "outside" context
7. Switch to context buffer you want to inspect (i.e. `context=foo` buffer for a function `foo`)
8. `M-x pynt-generate-worksheet RET` to generate a worksheet for this function

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
