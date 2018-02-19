{-# LANGUAGE FlexibleInstances, RankNTypes #-}

module DivisionTerm where

import Control.Monad (guard)



data Term = Lit Int
          | Term :+: Term
          | Term :/: Term
  deriving (Show)



eval :: Term -> Maybe Int
eval (Lit i) = return i
eval (m :+: n) = (+) <$> eval m <*> eval n
eval (m :/: n) = do n' <- eval n
                    guard (n' /= 0)
                    div <$> eval m <*> pure n'



class TermC a where
  lit :: Int -> a
  plus :: a -> a -> a
  divide :: a -> a -> a



instance TermC Term where
  lit i = Lit i
  plus m n = m :+: n
  divide m n = m :/: n

instance TermC (Maybe Int) where
  lit i = return i
  plus m n = (+) <$> m <*> n
  divide m n = do n' <- n
                  guard (n' /= 0)
                  div <$> m <*> pure n'



type FinalTerm = forall a. TermC a => a

prettyFinal :: FinalTerm -> String
prettyFinal m = show (m :: Term)

evalFinal :: FinalTerm -> Maybe Int
evalFinal m = m
