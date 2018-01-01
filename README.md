# PYNT (PYthon iNTeractive)

Emacs package for generating and interacting with a jupyter notebooks from existing code.

## Getting Started

### Prerequisites

- Emacs
- jupyter notebook
- EIN [[1]](http://millejoh.github.io/emacs-ipython-notebook/) and [[2]](https://github.com/millejoh/emacs-ipython-notebook)

### Using pynt

The first step is starting EIN just like you normally would. My workflow for that is the following.

1. `M-x ein:notebooklist-login RET 8888 RET my-secret-password RET`
2. `M-x ein:notebooklist-open RET 8888`
3. Create a new notebook or choose an existing one
4. Pull up your code side-by-side with the worksheet
5. `M-x ein:connect-to-notebook-buffer RET`
6. `M-x pynt-mode RET` to activate pynt-mode
7. `M-x pynt-generate-worksheet RET` to create blank worksheets for each function and "outside" context
8. Switch to context buffer you want to inspect (i.e. `context=foo` buffer for a function `foo`)
9. `M-x pynt-generate-worksheet RET` to generate a worksheet for this function
