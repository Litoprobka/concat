module Types where

import Relude
import qualified Data.HashMap.Strict as Map
import Optics ()

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

type WordAction = Repl ()

data Value
  = Num Int
  | Txt Text
  | Word' Text
  | CompiledWord WordAction
  | Quote' [Value]

data ReplState = ReplState
  { stack :: [Value]
  , dict :: Map.HashMap Text WordAction
  }
  deriving (Generic)

type Repl a = StateT ReplState IO a