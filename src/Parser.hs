{-# LANGUAGE ViewPatterns #-}
module Parser where

import Relude
import Types
import Data.Text qualified as Text
import Optics

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