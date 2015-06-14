{-# LANGUAGE OverloadedStrings #-}

module Redl.Compiler.Parser where

import qualified Data.Text as T
import Data.Attoparsec.Text as P
import Control.Applicative
import qualified Data.Scientific as S
import Data.String (IsString(..))


data Atom = Symbol Symbol
          | Keyword Keyword
          | Number S.Scientific
          | String T.Text
  deriving (Show, Read, Eq, Ord)

newtype Symbol = S T.Text
  deriving (Show, Read, Eq, Ord)
newtype Keyword = KW T.Text
  deriving (Show, Read, Eq, Ord)

data Sexp = Sexp [PSexp]
          | Atom Atom
  deriving (Show, Eq)

data PSexp = Maybe T.Text :-: Sexp
  deriving (Show, Eq)

instance IsString PSexp where
  fromString s = Nothing :-: Atom (Symbol $ S $ T.pack s)

sym (Nothing :-: Atom (Symbol s)) = Just s
sym _ = Nothing

slist (Nothing :-: Sexp l) = l
slist _ = []

sexp_prefix = choice $ map string ["#", "\\", "~", "!"]

blankspace = many' space
sexp_opening = char '(' <* blankspace
sexp_closing = blankspace *> char ')'

hexa = cnv <$> (string "0x" *>
                (many1' $ digit <|> choice (map char "aAbBcCdDeEfF")))
  where cnv t = S.scientific (read ("0x" ++ t)) 0

str = char '"' *> takeTill (== '"') <* char '"'

bare_symbol = choice $ [T.pack <$> many1' (letter <|> digit <|>
                                    (choice $ map char "_-+*/<>=.?"))] ++
                  map string ["§>", "§"]
keyword = char ':' *> (T.cons ':' <$> bare_symbol)
atom = Atom <$> ((Number <$> (hexa <|> scientific))
                 <|> (String <$> str)
                 <|> (Keyword . KW <$> keyword)
                 <|> (Symbol . S <$> bare_symbol))

list_sexp = do
  sexp_opening
  inner <- psexp `sepBy1` blankspace
  sexp_closing
  return $ Sexp inner

mb p = (Just <$> p) <|> pure Nothing

psexp = (:-:) <$> mb sexp_prefix
              <*> (list_sexp <|> atom)
