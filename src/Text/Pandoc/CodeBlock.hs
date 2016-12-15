module Text.Pandoc.CodeBlock where

import Text.Pandoc
import Data.Text(Text)
import qualified Data.Text as T
import Control.Lens

data CodeBlock = CodeBlock_
  { _attributes :: Attr
  , _body :: Text
  } deriving (Show, Eq)

attributes
  :: Functor f
  => (Attr -> f Attr) -> CodeBlock -> f CodeBlock
attributes f (CodeBlock_ a b) = fmap (\a' -> CodeBlock_ a' b) (f a)

body
  :: Functor f
  => (Text -> f Text) -> CodeBlock -> f CodeBlock
body f (CodeBlock_ a b) = fmap (\b' -> CodeBlock_ a b') (f b)

classes :: Lens' CodeBlock [String]
classes = attributes . _2

codeBlock :: CodeBlock -> Block
codeBlock cb = CodeBlock (_attributes cb) (T.unpack $ _body cb)

modClassesBody
  :: Functor m
  => ([String] -> Text -> m ([String], Text)) -> CodeBlock -> m Block
modClassesBody f cb = do
  f (view classes cb) (view body cb) <&>
    \(cs, b) -> codeBlock . set classes cs $ set body b cb
