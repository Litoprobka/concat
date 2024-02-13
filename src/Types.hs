module Types where

import Relude
import qualified Data.HashMap.Strict as Map
import Text.Show qualified as Show
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