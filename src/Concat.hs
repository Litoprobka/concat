{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module Concat where

import Data.HashMap.Strict qualified as Map
import GHC.Generics ()
import Optics
import Relude hiding (swap, tail)
import Relude.Unsafe (tail)
import Types
import Parser

eval :: ASTNode -> Repl ()
eval (NumLit n) = push $ Num n
eval (TextLit text) = push $ Txt text
eval (Word name) =
  preuse (#dict % ix name)
    >>= fromMaybe (error $ "Word " <> name <> " is not defined")
eval (Quote words') = push (Word' $ traverse_ eval words')

run :: Text -> IO [Value]
run code = case parseAST $ tokenize code of
  Nothing -> error "Parse error somewhere"
  Just ast -> fmap stack $ traverse_ eval ast `execStateT` ReplState{stack = [], dict = builtins}

builtins :: Map.HashMap Text WordAction
builtins =
  Map.fromList
    [ ("pop", void pop)
    , ("dup", dup)
    , ("swap", swap)
    , ("apply", apply)
    , ("if", ifWord)
    , ("when", push (Word' pass) >> ifWord)
    , ("unless", push (Word' pass) >> swap >> ifWord)
    , ("def", def)
    , (".", pop >>= print)
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

push :: Value -> Repl ()
push val = modifying' #stack (val :)

peek :: Repl Value
peek =
  preuse (#stack % _head)
    <&> fromMaybe (error "Attempted to pop empty stack")

pop :: Repl Value
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

apply :: Repl ()
apply = do
  Word' action <- pop
  action

ifWord :: Repl ()
ifWord = do
  Word' onFalse <- pop
  Word' onTrue <- pop
  Num cond <- pop
  if cond == 0
    then onFalse
    else onTrue

def :: Repl ()
def = do
  Word' quote <- pop
  Txt name <- pop
  modify' $ #dict % at name ?~ quote

binOp :: (Int -> Int -> Int) -> Repl ()
binOp op = do
  Num y <- pop
  Num x <- pop
  push $ Num $ x `op` y

logicOp :: (Int -> Int -> Bool) -> Repl ()
logicOp op = do
  Num y <- pop
  Num x <- pop
  push $ Num $ if x `op` y then 1 else 0