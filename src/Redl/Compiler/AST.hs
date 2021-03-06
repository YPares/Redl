{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Redl.Compiler.AST where


import qualified Data.Text as T
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

import Redl.Compiler.Parser


newtype Scope = Scope [(Symbol, Maybe AST)]
  deriving (Show)

type BoundedScope = [(Symbol, AST)]

-- | Splits the scope between unbounded and bounded variables.
split_scope :: Scope -> ([Symbol], BoundedScope)
split_scope (Scope l) = foldr go ([], []) l
  where go (s, Nothing) (unbounded, bounded) = (s:unbounded, bounded)
        go (s, Just v)  (unbounded, bounded) = (unbounded, (s, v):bounded)

newtype TypeHint = TH T.Text
  deriving (Show, Eq)

data AST = LocalScope Symbol Scope
         | Coroutine Symbol Scope
         | PrimCall Symbol (Maybe TypeHint) [AST]
         | ProcCall Symbol (Maybe TypeHint) [AST]
         | ExtHook T.Text TypeHint [AST]
         | Var Symbol
  deriving (Show)

{-
  Primitives are:

- def
- +, *, -, /, etc.
- §, §>, §mut
- Type hints
-}

reserved_symbols = map BSym ["def", "+", "*", "-", "/", "§", "§>", "§mut", "int", "rational", "bool", "string"]

type MonadAST m = (MonadError String m,
                   MonadReader BoundedScope -- ^ Global scope
                               m,
                   MonadState [Scope] -- ^ Local scopes, from
                                      -- innermost to outermost
                              m)

look_for_globals :: [SExp] -> BoundedScope
look_for_globals = foldr go []
  where go (SList ["def", Atom (Symbol var_name), val_expr]) scope =
          (var_name, undefined) : scope
        go _ scope = scope

make_ast :: MonadAST m
         => SExp -> m AST
make_ast (Atom a) = get >> undefined

