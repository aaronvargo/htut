{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Tut.Repl where

import Prelude hiding (fail)
import Tut.Misc
import Tut.Transformation
import Tut.Imports hiding (fail)
import Text.Pandoc
import Data.List (groupBy, delete)
import qualified Data.Text as T
import Control.Lens
import Data.Bool(bool)
import Data.Function(on)

--TODO allow eval of multiple inputs at once
data Repl m = Repl
  { langClasses :: [String]
  , prompt :: Text
  , eval :: Text -> m ReplOutput
  , commentPrefix :: Text
  }

data ReplOutput = ReplOutput
  { result :: Text
  , failed :: Bool
  }

data ReplError
  = UnexpectedError
  | UnexpectedSuccess
  deriving (Show, Eq)

--TODO improve error
class AsReplError e  where
  replError :: ReplError -> e

instance AsReplError String where
  replError UnexpectedError = "Unexpected repl error."
  replError UnexpectedSuccess = "Unexpected repl success."

replBlock
  :: forall e m. (MonadError e m, AsReplError e)
  => Repl m -> CodeBlock -> m Block
replBlock r cb =
  flip evalState cs $
  do silent <- flag "silent"
     invisible <- flag "invisible"
     fail <- flag "fail"
     book <- flag "book"
     multiline <- (book ||) <$> flag "multiline"
     plain <- flag "plain"
     woFlags <- get
     let addClasses
           | plain = id
           | otherwise = (langClasses r ++)
         ins
           -- why was this here?
           --  | silent = [[t]]
           | multiline = groupBy ((==) `on` T.null) (T.lines t)
           | otherwise = return <$> T.lines t
         f :: [Text] -> WriterT Any m [Text]
         f xs
           | all T.null xs = return xs
           | otherwise = do
             (ReplOutput o fl) <- lift $ eval r (unlines1 xs)
             let wPrmpt =
                   zipWith
                     (\a b -> prompt r <> a <> " " <> b)
                     (">" : repeat "|")
                     xs
             tell $ Any fl
             return $
               if book
                 then xs ++ ((\x -> commentPrefix r <> " " <> x) <$> T.lines o)
                 else wPrmpt ++ if T.null o then [] else [o]
     return $
       do (outs, Any fl) <- runWriterT (traverse f ins)
          when (fl /= fail) . throwError . replError $
            bool UnexpectedSuccess UnexpectedError fl
          let setBody
                | silent = id
                | otherwise = set body (unlines1 (join outs))
          return $
            if invisible
              then Null
              else codeBlock . set classes (addClasses woFlags) $ setBody cb
  where
    cs = view classes cb
    t = view body cb
    flag s = do
      modify (delete s)
      return $ s `elem` cs
