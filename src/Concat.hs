{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}

module Concat where

import Data.HashMap.Strict qualified as Map
import Data.Text qualified as Text
import GHC.Generics ()
import Optics
import Relude hiding (swap, tail)
import Relude.Unsafe (tail)
import Text.Show qualified as Show

data Token
  = WordOrNum Text
  | TextLit' Text
  | QuoteStart
  | QuoteEnd
  deriving (Show)

data ASTNode
  = NumLit Int
  | TextLit Text
  | Word Text
  | Quote [ASTNode]
  deriving (Show)

-- Handwritten parsing for the fun of it
-- the code is nowhere close to being nice, it feels like desugared parser combinators

tokenize :: Text -> [Token]
tokenize txtWithNewlines
  | Text.null txt = []
  | otherwise =
      filter (not . isEmpty) $
        let (token, restWithDelim) = Text.span (`notElem` splittingChars) txt
         in WordOrNum token : case Text.uncons restWithDelim of
              Nothing -> []
              Just (' ', rest) -> tokenize $ Text.dropWhile (== ' ') rest
              Just ('"', rest) ->
                let (textLit, rest') = Text.span (/= '"') rest
                 in TextLit' textLit : tokenize (Text.drop 1 rest')
              Just (bracket, rest) -> quoteToken bracket : tokenize rest
 where
  txt = txtWithNewlines & Text.words & Text.unwords

  quoteToken '[' = QuoteStart
  quoteToken _ = QuoteEnd

  isEmpty (WordOrNum word) = Text.null word
  isEmpty _ = False

  splittingChars = ['[', ']', '"', ' ']

parseAST :: [Token] -> Maybe [ASTNode]
parseAST [] = Just []
parseAST (token : rest) =
  withLits
    token
    (\ast -> (ast :) <$> parseAST rest) -- what to do after a literal
    Nothing -- what to do on QuoteEnd (] without a leading [)
    ( do
        -- what to do on QuoteStart
        (quote, rest') <- parseQuote rest
        ast <- parseAST rest'
        pure $ Quote quote : ast
    )
 where
  parseQuote :: [Token] -> Maybe ([ASTNode], [Token])
  parseQuote [] = Nothing
  parseQuote (token' : rest') =
    withLits
      token'
      parseAndCons
      (Just ([], rest'))
      ( do
          (nestedQuote, rest'') <- parseQuote rest'
          (quote, rest3) <- parseQuote rest''
          pure (Quote nestedQuote : quote, rest3)
      )
   where
    parseAndCons ast =
      over _1 (ast :) <$> parseQuote rest'

  withLits :: Token -> (ASTNode -> a) -> a -> a -> a
  withLits t f onQEnd onQStart = case t of
    WordOrNum (readText -> Just num) -> f $ NumLit num
    WordOrNum word -> f $ Word word
    TextLit' text -> f $ TextLit text
    QuoteEnd -> onQEnd
    QuoteStart -> onQStart

  readText :: Read a => Text -> Maybe a
  readText = readMaybe . Text.unpack

type WordAction = Repl ()

data Value
  = Num Int
  | Txt Text
  | Word' WordAction

instance Show Value where
  show (Num n) = show n
  show (Txt text) = show text
  show (Word' _) = "<word>"

data ReplState = ReplState
  { stack :: [Value]
  , dict :: Map.HashMap Text WordAction
  }
  deriving (Generic)

type Repl a = StateT ReplState IO a

eval :: ASTNode -> Repl ()
eval (NumLit n) = push $ Num n
eval (TextLit text) = push $ Txt text
eval (Word name) =
  preuse (#dict % ix name)
    >>= fromMaybe (error $ "Word " <> name <> " is not defined")
eval (Quote words') = push (Word' $ traverse_ eval words')

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

run :: Text -> IO [Value]
run code = case parseAST $ tokenize code of
  Nothing -> error "Parse error somewhere"
  Just ast -> fmap stack $ traverse_ eval ast `execStateT` ReplState{stack = [], dict = builtins}

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