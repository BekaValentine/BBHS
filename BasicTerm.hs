{-# LANGUAGE RankNTypes, TypeSynonymInstances, FlexibleInstances #-}

module BasicTerm where



data Term = Lit Int
          | Term :+: Term
          | Term :*: Term
  deriving (Show)



eval :: Term -> Int
eval (Lit i) = i
eval (m :+: n) = eval m + eval n
eval (m :*: n) = eval m * eval n



data Frame = PlusL Term | PlusR Int
           | TimesL Term | TimesR Int

type Stack = [Frame]

data Machine = Entering Stack Term
             | Exiting Stack Int

run :: Machine -> Int
run (Entering s (Lit i)) = run (Exiting s i)
run (Entering s (m :+: n)) = run (Entering (PlusL n:s) m)
run (Entering s (m :*: n)) = run (Entering (TimesL n:s) m)
run (Exiting (PlusL n:s) m') = run (Entering (PlusR m':s) n)
run (Exiting (PlusR m':s) n') = run (Exiting s (m' + n'))
run (Exiting (TimesL n:s) m') = run (Entering (TimesR m':s) n)
run (Exiting (TimesR m':s) n') = run (Exiting s (m' * n'))
run (Exiting [] i) = i




data Instr = LIT Int | PLUS | TIMES
  deriving (Show)

type Code = [Instr]

compile :: Term -> Code
compile (Lit i) = [LIT i]
compile (m :+: n) = compile m ++ compile n ++ [PLUS]
compile (m :*: n) = compile m ++ compile n ++ [TIMES]

type ReturnStack = [Int]

execute :: ReturnStack -> Code -> Int
execute s (LIT i:c) = execute (i:s) c
execute (n':m':s) (PLUS:c) = execute (m' + n' : s) c
execute (n':m':s) (TIMES:c) = execute (m' * n' : s) c




class TermC a where
  lit :: Int -> a
  plus :: a -> a -> a
  times :: a -> a -> a



instance TermC Term where
  lit i = Lit i
  plus m n = m :+: n
  times m n = m :*: n

instance TermC Int where
  lit i = i
  plus m n = m + n
  times m n = m * n

instance TermC Code where
  lit i = [LIT i]
  plus m n = m ++ n ++ [PLUS]
  times m n = m ++ n ++ [TIMES]



type FinalTerm = forall a. TermC a => a

prettyFinal :: FinalTerm -> String
prettyFinal m = show (m :: Term)

evalFinal :: FinalTerm -> Int
evalFinal m = m

compileFinal :: FinalTerm -> Code
compileFinal m = m :: Code
