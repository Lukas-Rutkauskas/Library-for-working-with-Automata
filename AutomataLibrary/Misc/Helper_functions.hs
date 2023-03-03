module AutomataLibrary.Misc.Helper_functions where

hasAny [] _          = False
hasAny _ []          = False
hasAny search (x:xs) = (x `elem` search) || hasAny search xs

cartProd xs ys = [(x,y) | x <- xs, y <- ys]