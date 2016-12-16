Tool for including code in documents. Extends
[pandoc](http://pandoc.org/) with the ability to include blocks of
source code from other files, as well as evaluate haskell code in ghci.
Partially inspired by [tut](https://github.com/tpolecat/tut).

Htut can be used either as an executable, or as a library which can be
used with [Hakyll](https://jaspervdj.be/hakyll/).

To use it with hakyll, `defaultHakyllTut` can be passed as an argument
to `pandocCompilerWithTransformM`:

``` {.haskell}
λ> import Hakyll

λ> import Tut.Hakyll(defaultHakyllTut)

λ> :t defaultHakyllTut
defaultHakyllTut :: Walkable Block b => b -> Compiler b

λ> :t pandocCompilerWithTransformM
pandocCompilerWithTransformM
  :: ReaderOptions
     -> WriterOptions
     -> (Pandoc -> Compiler Pandoc)
     -> Compiler (Item String)

λ> :t pandocCompilerWithTransformM def def defaultHakyllTut
pandocCompilerWithTransformM def def defaultHakyllTut
  :: Compiler (Item String)
```