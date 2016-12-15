Hello
=====

``` {.haskell}
λ> import Hakyll

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