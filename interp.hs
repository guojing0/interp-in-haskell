{-# LANGUAGE FlexibleInstances #-}

module Interp where
import ExprT
import Parser

class Expr a where
    lit :: a -> ExprT
    add :: a -> a -> ExprT
    mul :: a -> a -> ExprT

-- mul (add (lit 2) (lit 3)) (lit 4) :: ExprT

instance Expr Integer where
    lit n = Lit n
    add x y = Add (Lit x) (Lit y)
    mul x y = Mul (Lit x) (Lit y)

eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add x y) = (eval x) + (eval y)
eval (Mul x y) = (eval x) * (eval y)

evalStr :: String -> Maybe Integer
evalStr exprStr = case (parseExp Lit Add Mul exprStr) of
    Just x  -> Just $ eval x
    Nothing -> Nothing

