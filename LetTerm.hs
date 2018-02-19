{-# LANGUAGE RankNTypes, TypeSynonymInstances, FlexibleInstances #-}

module LetTerm where

import Control.Monad.Trans.Reader



data Term = Lit Int
          | Term :+: Term
          | Var String
          | Let String Term Term
  deriving (Show)



type Env = [(String,Int)]

eval :: Term -> Reader Env Int
eval (Lit i) = return i
eval (m :+: n) = (+) <$> eval m <*> eval n
eval (Var x) =
  do env <- ask
     case lookup x env of
       Nothing -> error ("Unbound variable: " ++ x)
       Just m -> return m



type Context = [String]

scopeCheck :: Term -> Reader Context Bool
scopeCheck (Lit i) = return True
scopeCheck (m :+: n) = (&&) <$> scopeCheck m <*> scopeCheck n
scopeCheck (Var x) = do ctx <- ask
                        return (x `elem` ctx)
scopeCheck (Let x m n) = (&&) <$> scopeCheck m <*> local (x:) (scopeCheck n)



class TermC a where
  lit :: Int -> a
  plus :: a -> a -> a
  var :: String -> a
  lett :: String -> a -> a -> a



instance TermC Term where
  lit i = Lit i
  plus m n = m :+: n
  var x = Var x
  lett x m n = Let x m n

instance TermC (Reader Env Int) where
  lit i = return i
  plus m n = (+) <$> m <*> n
  var x = do env <- ask
             case lookup x env of
               Nothing -> error ("Unbound variable: " ++ x)
               Just m -> return m
  lett x m n = do m' <- m
                  local ((x,m'):) n

instance TermC (Reader Context Bool) where
  lit i = return True
  plus m n = (&&) <$> m <*> n
  var x = do ctx <- ask
             return (x `elem` ctx)
  lett x m n = (&&) <$> m <*> local (x:) n




type FinalTerm = forall a. TermC a => a

prettyFinal :: FinalTerm -> String
prettyFinal m = show (m :: Term)

evalFinal :: FinalTerm -> Int
evalFinal m = runReader (m :: Reader Env Int) []

scopeCheckFinal :: FinalTerm -> Bool
scopeCheckFinal m = runReader (m :: Reader Context Bool) []
