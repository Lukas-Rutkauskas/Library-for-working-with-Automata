module DFA_Test where


import Data.List


-- Data type for epsilon transitions and normal transitions | isomorphic to Maybe
data Transition a = Epsilon | Symbol a


hasAny [] _          = False             -- An empty search list: always false
hasAny _ []          = False             -- An empty list to scan: always false
hasAny search (x:xs) = if x `elem` search then True else hasAny search xs


-- Data type of a Deterministic Finite Automata polymorphically parameterized
-- with the type parameters 'symbols' and 'states', for the type of symbols and states respectively.

data DFA symbols states = Dfa
                        [states]                            -- Finite set of states
                        [symbols]                           -- Finite set of symbols, i.e. the alphabet
                        states                              -- Starting state
                        [states]                            -- Finite set of final/accepting states
                        (states -> symbols -> states)       -- Transition function


-- Executes the run of a DFA on the input word by starting at the start state
-- and taking transitions according to the transition function.
-- Return the end state when the input has been fully expended.

dfaRun ::  states                                           -- Start state
        -> [symbol]                                         -- Input
        -> (states -> symbol -> states)                     -- Transition function
        -> states                                           -- End state
dfaRun st input trans = foldl trans st input


-- Checks whether the given DFA accepts or rejects the given input.

dfaAccept :: Eq states => DFA symbols states
            -> [symbols]
            -> Bool
dfaAccept (Dfa _ _ start final trans) input = elem (dfaRun start input trans) final


-- Data type of a Non-Deterministic Finite Automata polymorphically parameterized
-- with the type parameters 'symbols' and 'states', for the type of symbols and states respectively.

data NFA symbols states = Nfa
                        [states]                                        -- Finite set of states
                        [Transition symbols]                            -- Finite set of symbols, i.e. the alphabet
                        states                                          -- Starting state
                        [states]                                        -- Finite set of final/accepting states
                        (states -> Transition symbols -> [states])      -- Transition function


-- Executes the run of an NFA on the input word by starting at the start state(s)
-- and taking transitions according to the transition function.
-- Return the end states when the input has been fully expended.

nfaRun :: Eq states => (states -> Transition symbol -> [states])        -- Transition function
        -> [Transition symbol]                                          -- Input
        -> [states]                                                     -- Start states
        -> [states]                                                     -- End states
nfaRun trans (x:xs) st = 
        let 
            epsilonB     = eClose trans st                              -- take epsilon transitions, returns all possible starting states before reading any input
            step         = nub (concatMap (`trans` x) epsilonB)         -- transition upon reading an input symbol, starting from all states from `epsilonB`, returns all possible states
            epsilonFinal = eClose trans step                            -- ECLOSE of step`
        in
            nfaRun trans xs epsilonFinal                                -- calls itself with the rest of the input and all the possible end states after reading the current symbol
nfaRun trans [] st = eClose trans st


-- Calculates the ECLOSE of a set of states

eClose :: Eq states => (states -> Transition symbol -> [states])        -- Transition function
                -> [states]                                             -- Start states
                -> [states]                                             -- End states
eClose trans st = 
        let 
            curr = nub (concatMap (`trans` Epsilon) st)
        in
            if curr == st then st else eClose trans curr


-- Checks whether the given NFA accepts or rejects the given input.

nfaAccept :: Eq states => NFA symbols states
            -> [Transition symbols]
            -> Bool
nfaAccept (Nfa _ _ start final trans) input = hasAny (nfaRun trans input [start]) final


-- TESTS

setOfStates1  = ["s0","s1","s2","d"]
setOfSymbols1 = [0,1]
startState1   = "s0"
finalStates1  = ["s2"]
transitionF1 "s0" 0 = "s1"
transitionF1 "s0" 1 = "d"
transitionF1 "s1" 0 = "s1"
transitionF1 "s1" 1 = "s2"
transitionF1 "s2" 0 = "s1"
transitionF1 "s2" 1 = "s2"
transitionF1 "d"  0 = "d"
transitionF1 "d"  1 = "d"

-- DFA that accepts words comprised of 0s and 1s that start with a 0 and end with a 1.
testDFA = Dfa setOfStates1 setOfSymbols1 startState1 finalStates1 transitionF1


setOfStates2  = ["s0","s1","s2"]
setOfSymbols2 = [Symbol 0, Symbol 1]
startState2   = "s0"
finalStates2  = ["s2"]
transitionF2 "s0" (Symbol 0) = ["s1"]
transitionF2 "s0" (Symbol 1) = []
transitionF2 "s0" Epsilon    = ["s0","s1"]
transitionF2 "s1" (Symbol 0) = ["s1"]
transitionF2 "s1" (Symbol 1) = ["s1","s2"]
transitionF2 "s1" Epsilon    = ["s1"]
transitionF2 "s2" (Symbol 0) = []
transitionF2 "s2" (Symbol 1) = []
transitionF2 "s2" Epsilon    = ["s2"]

-- NFA that accepts words comprised of 0s and 1s that end with a 1.
testNFA = Nfa setOfStates2 setOfSymbols2 startState2 finalStates2 transitionF2


setOfStates3  = ["s0","s1","s2","s3","s4"]
setOfSymbols3 = [Symbol 0, Symbol 1]
startState3  = "s0"
finalStates3  = ["s1","s3"]
transitionF3 "s0" (Symbol 0) = []
transitionF3 "s0" (Symbol 1) = []
transitionF3 "s0" Epsilon    = ["s0","s1","s3"]
transitionF3 "s1" (Symbol 0) = ["s2"]
transitionF3 "s1" (Symbol 1) = ["s1"]
transitionF3 "s1" Epsilon    = ["s1"]
transitionF3 "s2" (Symbol 0) = ["s1"]
transitionF3 "s2" (Symbol 1) = ["s2"]
transitionF3 "s2" Epsilon    = ["s2"]
transitionF3 "s3" (Symbol 0) = ["s3"]
transitionF3 "s3" (Symbol 1) = ["s4"]
transitionF3 "s3" Epsilon    = ["s3"]
transitionF3 "s4" (Symbol 0) = ["s4"]
transitionF3 "s4" (Symbol 1) = ["s3"]
transitionF3 "s4" Epsilon    = ["s4"]

-- NFA that accepts words comprised of 0s and 1s that have an even number of 0s or an even number of 1s
testNFA2 = Nfa setOfStates3 setOfSymbols3 startState3 finalStates3 transitionF3