{-# LANGUAGE OverloadedStrings #-}

module Redl.Compiler.Parser where

import qualified Data.Text as T
import Data.Attoparsec.Text as P
import Control.Applicative
import qualified Data.Scientific as S
import Data.String (IsString(..))


data Atom = Symbol Symbol
          | PrefixedSymbol T.Text Symbol
          | Number S.Scientific
          | String T.Text
  deriving (Show, Read, Eq, Ord)

data Symbol = BSym T.Text
            | NsSym [T.Text] T.Text
  deriving (Show, Read, Eq, Ord)

data SExp = SList [SExp]
          | PrefixedSList T.Text [SExp]
          | Atom Atom
  deriving (Show, Eq)

instance IsString SExp where
  fromString s = case parseOnly sexp (T.pack s) of
                  Right s -> s
                  _ -> error "No parse."

sexp_prefix = choice $ map string ["#", "\\", "~", "!", "'", ","]
symbol_prefix = choice $ map string ["!", "#", "~", "'", ","]

blankspace = many' space
sexp_opening = char '(' <* blankspace
sexp_closing = blankspace *> char ')'

hexa = cnv <$> (string "0x" *>
                (many1' $ digit <|> choice (map char "aAbBcCdDeEfF")))
  where cnv t = S.scientific (read ("0x" ++ t)) 0

str = char '"' *> takeTill (== '"') <* char '"'

bare_symbol = choice $
  [T.pack <$> many1' (letter <|> digit <|>
                      (choice $ map char ok_chars))] ++
   map string ["§>", "§"]
  where ok_chars = "_-+*/~<>=.?"

symbol = (NsSym <$> path <*> bare_symbol)
         <|> (BSym <$> bare_symbol)
  where path = (:) <$> ((bare_symbol <|> pure "") <* char ':')
                   <*> (path <|> pure [])

prefixed_symbol = PrefixedSymbol <$> symbol_prefix <*> symbol

atom = (Number <$> (hexa <|> scientific))
       <|> (String <$> str)
       <|> prefixed_symbol
       <|> (Symbol <$> symbol)

list_sexp = do
  sexp_opening
  inner <- sexp `sepBy1` blankspace
  sexp_closing
  return inner

sexp = ((PrefixedSList <$> sexp_prefix <*> list_sexp)
        <|> (SList <$> list_sexp))
       <|> (Atom <$> atom)
