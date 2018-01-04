# PYNT (PYthon iNTeractive)

Emacs package for generating and interacting with a jupyter notebooks from existing code.

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

## TODO

- ~~Make a screencast demonstrating the functionality~~
- Add support for other language constructs (e.g. classes and context managers)
- Add buffer narrowing option so one can focus on just a single cell at a time
- Factor out notebook generator as a standalone utility separate from emacs
- Add gutter toggle for mocking out calls that you don't want to happen (long-running or side-effecting behavior)
- Add better support for integration with unit tests
