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

nodeToValue :: ASTNode -> Value
nodeToValue (NumLit n) = Num n
nodeToValue (TextLit text) = Txt text
nodeToValue (Quote nodes) = Quote' $ map nodeToValue nodes
nodeToValue (Word name) = Word' name

eval :: Value -> Repl ()
eval (CompiledWord action) = action
eval (Word' name) =
  preuse (#dict % ix name)
    >>= fromMaybe (fail' $ "Word " <> name <> " is not defined")
eval nonWord = push nonWord

fail' :: MonadFail m => Text -> m a
fail' = fail . toString

run :: Text -> IO [Value]
run code = case parseAST $ tokenize code of
  Nothing -> fail' "Parse error somewhere"
  Just ast -> fmap stack $ traverse_ (eval . nodeToValue) ast `execStateT` ReplState{stack = [], dict = builtins}

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
    , ("when", push noop >> ifWord)
    , ("unless", push noop >> swap >> ifWord)
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

noop :: Value
noop = CompiledWord pass

push :: Value -> Repl ()
push val = modifying' #stack (val :)

peek :: Repl Value
peek = do
  mbHead <- preuse (#stack % _head)
  whenNothing mbHead (fail' "Attempted to pop empty stack")

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

apply :: Value -> Repl ()
apply (Quote' values) = traverse_ eval values
apply word@Word'{} = eval word
apply cword@CompiledWord{} = eval cword
apply other = fail' $ "Attempted to call " <> prettyNode other

-- there should be a way to factor out this boilerplate

asNum :: Value -> Repl Int
asNum (Num n) = pure n
asNum other = fail' $ "Expected a number, got " <> prettyNode other

asText :: Value -> Repl Text
asText (Txt text) = pure text
asText other = fail' $ "Expected a text, got " <> prettyNode other

asQuote :: Value -> Repl [Value]
asQuote (Quote' quote) = pure quote
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
  push $ Quote' newList

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
  peekQuote
  value <- pop
  push (CompiledWord $ void pop) >> onQuote
  push value

pushQuote :: Repl ()
pushQuote = do
  value <- pop
  list <- asQuote =<< pop
  push $ Quote' $ value : list

boolToNum :: Bool -> Value
boolToNum False = Num 0
boolToNum True = Num 1

isEmpty :: Repl ()
isEmpty = do
  list <- asQuote =<< pop
  push $ boolToNum $ null list

local' :: Value -> Repl ()
local' code = do
  dict <- use #dict
  apply code
  modify' $ #dict .~ dict

binOp :: (Int -> Int -> Int) -> Repl ()
binOp op = do
  y <- asNum =<< pop
  x <- asNum =<< pop
  push $ Num $ x `op` y

logicOp :: (Int -> Int -> Bool) -> Repl ()
logicOp op = do
  y <- asNum =<< pop
  x <- asNum =<< pop
  push $ boolToNum $ x `op` y

prettyPrint :: [Value] -> Text
prettyPrint ast = "[" <> Text.unwords (map prettyNode ast) <> "]"

prettyNode :: Value -> Text
prettyNode (Num n) = show n
prettyNode (Txt text) = show text
prettyNode (Word' name) = name
prettyNode CompiledWord{} = "<word>"
prettyNode (Quote' quote) = prettyPrint quote