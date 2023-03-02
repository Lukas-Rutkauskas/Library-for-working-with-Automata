module AutomataLibrary.Misc.Data_types where

data Transition a = Epsilon | Symbol a
    deriving (Show,Eq)

data Tree a = Nil | Node a (Tree a) (Tree a)
    deriving (Show,Eq)

data Move = L | R | S
    deriving (Show,Eq)