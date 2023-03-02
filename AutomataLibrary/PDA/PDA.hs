module AutomataLibrary.PDA.PDA where


import AutomataLibrary.Misc.Data_types
import Data.List

data PDA symbol state = Pda
                        [state]                                                                                 -- Finite set of states
                        [Transition symbol]                                                                     -- Finite set of symbols (alphabet)
                        state                                                                                   -- Starting state
                        [state]                                                                                 -- Finite set of final/accepting states
                        (Transition symbol -> Transition symbol -> state -> [(state,Transition symbol)])        -- Transition function


pdaRun :: (Eq symbol, Eq state) => (Transition symbol -> Transition symbol -> state -> [(state,Transition symbol)])         -- Transition function
        -> [Transition symbol]                                                                                              -- Input
        -> state                                                                                                            -- Start state
        -> [Transition symbol]                                                                                              -- Stack
        -> [state]                                                                                                          -- End states
pdaRun _ [] start [] = [start]
pdaRun trans [] start (y:ys) =
    let
        three   = trans Epsilon Epsilon start
        four    = trans Epsilon y start
        three'  = pdaHelper three trans [] (y:ys)
        four'   = pdaHelper four trans [] ys
    in
        three' ++ four'
pdaRun trans (x:xs) start []     = 
    let
        one     = trans x Epsilon start
        three   = trans Epsilon Epsilon start
        one'    = pdaHelper one trans xs []
        three'  = pdaHelper three trans (x:xs) []
    in
        one' ++ three'
pdaRun trans (x:xs) start (y:ys) = 
    let
        one     = trans x Epsilon start
        two     = trans x y start
        three   = trans Epsilon Epsilon start
        four    = trans Epsilon y start
        one'    = pdaHelper one trans xs (y:ys)
        two'    = pdaHelper two trans xs ys
        three'  = pdaHelper three trans (x:xs) (y:ys)
        four'   = pdaHelper four trans (x:xs) ys
    in
        one' ++ two' ++ three' ++ four'


pdaHelper :: (Eq symbol, Eq state) => [(state,Transition symbol)]
         -> (Transition symbol -> Transition symbol -> state -> [(state,Transition symbol)]) 
         -> [Transition symbol]
         -> [Transition symbol]
         -> [state]
pdaHelper [] _ _ _ = []
pdaHelper (p:ps) trans input stack 
    | snd p == Epsilon  = nub (pdaRun trans input (fst p) stack ++ pdaHelper ps trans input stack)
    | otherwise         = nub (pdaRun trans input (fst p) (snd p : stack) ++ pdaHelper ps trans input stack)