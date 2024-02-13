{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Concat where

import Data.HashMap.Strict qualified as Map
import Data.Text qualified as Text
import GHC.Generics ()
import Optics
import Parser
import Relude hiding (swap, tail)
import Relude.Unsafe (tail)
import Types

eval :: ASTNode -> Repl ()
eval (Word name) =
  preuse (#dict % ix name)
    >>= fromMaybe (fail' $ "Word " <> name <> " is not defined")
eval nonWord = push nonWord

fail' :: MonadFail m => Text -> m a
fail' = fail . toString

run :: Text -> IO [ASTNode]
run code = case parseAST $ tokenize code of
  Nothing -> fail' "Parse error somewhere"
  Just ast -> fmap stack $ traverse_ eval ast `execStateT` ReplState{stack = [], dict = builtins}

builtins :: Map.HashMap Text WordAction
builtins =
  Map.fromList
    [ ("pop", void pop)
    , ("dup", dup)
    , ("swap", swap)
    , ("apply", pop >>= apply)
    , ("if", ifWord)
    , ("when", push (Quote []) >> ifWord)
    , ("unless", push (Quote []) >> swap >> ifWord)
    , ("def", def)
    , (".", pop >>= putTextLn . prettyNode)
    , ("+", binOp (+))
    , ("-", binOp (-))
    , ("*", binOp (*))
    , ("/", binOp div)
    , (">", logicOp (>))
    , ("<", logicOp (<))
    , (">=", logicOp (>=))
    , ("<=", logicOp (<=))
    , ("==", logicOp (==))
    , ("!=", logicOp (/=))
    ]

push :: ASTNode -> Repl ()
push val = modifying' #stack (val :)

peek :: Repl ASTNode
peek = do
  mbHead <- preuse (#stack % _head)
  whenNothing mbHead (fail' "Attempted to pop empty stack")

pop :: Repl ASTNode
pop = do
  result <- peek
  modifying' #stack tail
  pure result

dup :: Repl ()
dup = do
  val <- peek
  push val

swap :: Repl ()
swap = do
  y <- pop
  x <- pop
  push y
  push x

apply :: ASTNode -> Repl ()
apply (Quote values) = traverse_ eval values
apply word@Word{} = eval word
apply other = fail' $ "Attempted to call " <> prettyNode other

asNum :: ASTNode -> Repl Int
asNum (NumLit n) = pure n
asNum other = fail' $ "Expected a number, got " <> prettyNode other

asText :: ASTNode -> Repl Text
asText (TextLit text) = pure text
asText other = fail' $ "Expected text, got " <> prettyNode other

ifWord :: Repl ()
ifWord = do
  onFalse <- pop
  onTrue <- pop
  cond <- asNum =<< pop
  apply $
    if cond == 0
      then onFalse
      else onTrue

def :: Repl ()
def = do
  quote <- pop
  name <- asText =<< pop
  modify' $ #dict % at name ?~ apply quote

binOp :: (Int -> Int -> Int) -> Repl ()
binOp op = do
  y <- asNum =<< pop
  x <- asNum =<< pop
  push $ NumLit $ x `op` y

logicOp :: (Int -> Int -> Bool) -> Repl ()
logicOp op = do
  y <- asNum =<< pop
  x <- asNum =<< pop
  push $ NumLit $ if x `op` y then 1 else 0

prettyPrint :: [ASTNode] -> Text
prettyPrint ast = "[" <> Text.unwords (map prettyNode ast) <> "]"

prettyNode :: ASTNode -> Text
prettyNode (NumLit n) = show n
prettyNode (TextLit text) = show text
prettyNode (Word name) = name
prettyNode (Quote quote) = prettyPrint quote