module AutomataLibrary.TM.TM where

import AutomataLibrary.Misc.Data_types

data TM symbol state = Tm 
                        [state]                                     -- set of states
                        [symbol]                                    -- input alphabet
                        state                                       -- starting state
                        state                                       -- accept state
                        state                                       -- reject state
                        ((state,symbol) -> (state,symbol,Move))     -- transition function
                        state                                       -- current state
                        ([symbol],[symbol])                         -- tape
                        symbol                                      -- head value


tmRun :: (Eq symbol, Eq state) => TM symbol state 
    -> TM symbol state
tmRun (Tm states alphabet start acc rej trans curr tape hd) = 
    let
        (newSt,write,move) = trans (curr,hd)
        (newTape,newHead) = tmMove write tape move
    in
        Tm states alphabet start acc rej trans newSt newTape newHead


tmMove :: symbol -> ([symbol],[symbol]) -> Move -> (([symbol],[symbol]),symbol)
tmMove hd (left,right) move = case move of
    L ->
        let
            newHead  = last left
            newLeft  = take (length left - 1) left
            newRight = hd : right
        in
            ((newLeft,newRight),newHead)
    R ->
        let
            newHead  = head right
            newLeft  = left ++ [hd]
            newRight = drop 1 right
        in
            ((newLeft,newRight),newHead)
    S -> ((left,right),hd)


tmAccept :: (Eq symbol, Eq state) => TM symbol state -> Bool
tmAccept (Tm states alphabet start acc rej trans curr tape hd)
    | curr == acc || curr == rej    = curr == acc
    | otherwise                     = tmAccept $ tmRun (Tm states alphabet start acc rej trans curr tape hd)
