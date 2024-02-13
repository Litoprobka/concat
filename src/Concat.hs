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
    , ("swapd", swapd)
    , ("rot", rot)
    , ("rotd", rotd)
    , ("apply", pop >>= apply)
    , ("if", ifWord)
    , ("when", push (Quote []) >> ifWord)
    , ("unless", push (Quote []) >> swap >> ifWord)
    , ("def", def)
    , ("def-global", defGlobal)
    , ("on-quote", onQuote)
    , ("peek-quote", peekQuote)
    , ("pop-quote", popQuote)
    , ("push-quote", pushQuote)
    , ("empty?", isEmpty)
    , ("local", pop >>= local')
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

swapd :: Repl ()
swapd = do
  z <- pop
  y <- pop
  x <- pop
  push y
  push x
  push z

rot :: Repl ()
rot = do
  z <- pop
  y <- pop
  x <- pop
  push y
  push z
  push x

rotd :: Repl ()
rotd = do
  z <- pop
  y <- pop
  x <- pop
  w <- pop
  push x
  push y
  push w
  push z

apply :: ASTNode -> Repl ()
apply (Quote values) = traverse_ eval values
apply word@Word{} = eval word
apply other = fail' $ "Attempted to call " <> prettyNode other

-- there should be a way to factor out this boilerplate

asNum :: ASTNode -> Repl Int
asNum (NumLit n) = pure n
asNum other = fail' $ "Expected a number, got " <> prettyNode other

asText :: ASTNode -> Repl Text
asText (TextLit text) = pure text
asText other = fail' $ "Expected a text, got " <> prettyNode other

asQuote :: ASTNode -> Repl [ASTNode]
asQuote (Quote quote) = pure quote
asQuote other = fail' $ "Expected a quote, got " <> prettyNode other

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
  modify' $ #dict % at name ?~ local' quote

defGlobal :: Repl ()
defGlobal = do
  quote <- pop
  name <- asText =<< pop
  modify' $ #dict % at name ?~ apply quote

onQuote :: Repl ()
onQuote = do
  action <- pop
  list <- asQuote =<< pop
  prevStack <- use #stack

  modify' $ #stack .~ list
  apply action
  newList <- use #stack

  modify' $ #stack .~ prevStack
  push $ Quote newList

peekQuote :: Repl ()
peekQuote = do
  list <- asQuote =<< peek
  case list of
    [] -> fail' "Attempted to pop empty quote"
    x : _ -> push x

-- popQuote can be imlemented in terms of peekQuote, but we'd have to
-- use Quote [Word "peekQuote"], which may be overridden by user code
popQuote :: Repl ()
popQuote = do
  list <- asQuote =<< pop
  case list of
    [] -> fail' "Attempted to pop empty quote"
    x : xs -> push (Quote xs) >> push x

pushQuote :: Repl ()
pushQuote = do
  value <- pop
  list <- asQuote =<< pop
  push $ Quote $ value : list

boolToNum :: Bool -> ASTNode
boolToNum False = NumLit 0
boolToNum True = NumLit 1

isEmpty :: Repl ()
isEmpty = do
  list <- asQuote =<< pop
  push $ boolToNum $ null list

local' :: ASTNode -> Repl ()
local' code = do
  dict <- use #dict
  apply code
  modify' $ #dict .~ dict

binOp :: (Int -> Int -> Int) -> Repl ()
binOp op = do
  y <- asNum =<< pop
  x <- asNum =<< pop
  push $ NumLit $ x `op` y

logicOp :: (Int -> Int -> Bool) -> Repl ()
logicOp op = do
  y <- asNum =<< pop
  x <- asNum =<< pop
  push $ boolToNum $ x `op` y

prettyPrint :: [ASTNode] -> Text
prettyPrint ast = "[" <> Text.unwords (map prettyNode ast) <> "]"

prettyNode :: ASTNode -> Text
prettyNode (NumLit n) = show n
prettyNode (TextLit text) = show text
prettyNode (Word name) = name
prettyNode (Quote quote) = prettyPrint quote