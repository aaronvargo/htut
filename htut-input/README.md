---
ghci: '.'
---

Tool for including code in documents. Extends [pandoc](http://pandoc.org/) with the ability to include blocks of source code from other files, as well as evaluate haskell code in ghci. Partially inspired by [tut](https://github.com/tpolecat/tut).

Can be used either as an executable, or as a library which can be used with [Hakyll](https://jaspervdj.be/hakyll/).

This README and `example.md` were generated from the files in `htut-input/`, by running htut in the directory of this project.

To use htut with hakyll, `defaultHakyllTut` can be passed as an argument to `pandocCompilerWithTransformM`:

~~~ghci
import Hakyll

import Tut.Hakyll(defaultHakyllTut)

:t defaultHakyllTut

:t pandocCompilerWithTransformM

:t pandocCompilerWithTransformM def def defaultHakyllTut
~~~
