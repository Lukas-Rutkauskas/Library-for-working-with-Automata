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


aBcTm :: TM Char String
aBcTm = Tm states symbols "q0" "q5" "qr" delta "q0" ([], input) (head input)
  where
    states = ["q0", "q1", "q2", "q3", "q4", "q5", "qr"]
    symbols = ['a', 'b', 'c', ' ', 'x', 'y', 'z']
    input = "aaabbbbcccc "
    delta ("q0", 'a') = ("q1", 'x', R)
    delta ("q0", 'y') = ("q4", 'y', R)
    delta ("q1", 'a') = ("q1", 'a', R)
    delta ("q1", 'y') = ("q1", 'y', R)
    delta ("q1", 'b') = ("q2", 'y', R)
    delta ("q2", 'b') = ("q2", 'b', R)
    delta ("q2", 'z') = ("q2", 'z', R)
    delta ("q2", 'c') = ("q3", 'z', L)
    delta ("q3", 'b') = ("q3", 'b', L)
    delta ("q3", 'y') = ("q3", 'y', L)
    delta ("q3", 'a') = ("q3", 'a', L)
    delta ("q3", 'z') = ("q3", 'z', L)
    delta ("q3", 'x') = ("q0", 'x', R)
    delta ("q4", 'y') = ("q4", 'y', R)
    delta ("q4", 'z') = ("q4", 'z', R)
    delta ("q4", ' ') = ("q5", ' ', L)
    delta (_, sym) = ("qr", sym, S)   