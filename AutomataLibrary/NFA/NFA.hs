module AutomataLibrary.NFA.NFA where


import AutomataLibrary.Misc.Data_types
import AutomataLibrary.Misc.Helper_functions
import Data.List

-- Data type of a Non-Deterministic Finite Automata polymorphically parameterized
-- with the type parameters 'symbol' and 'state', for the type of symbols and states respectively.

data NFA symbol state = Nfa
                        [state]                                                     -- Finite set of states
                        [Transition symbol]                                         -- Finite set of symbols (alphabet)
                        state                                                       -- Starting state
                        [state]                                                     -- Finite set of final/accepting states
                        (state -> Transition symbol -> [state])                     -- Transition function


-- Executes the run of an NFA on the input word by starting at the start state(s)
-- and taking transitions according to the transition function.
-- Return the end states when the input has been fully expended.

nfaRun :: (Eq symbol, Eq state) => (state -> Transition symbol -> [state])          -- Transition function
        -> [Transition symbol]                                                      -- Input
        -> [state]                                                                  -- Start states
        -> [state]                                                                  -- End states
nfaRun trans (x:xs) st = 
        let 
            epsilonB     = eClose trans st                                          -- ECLOSE of starting states
            step         = nub (concatMap (`trans` x) epsilonB)                     -- transition upon reading an input symbol, starting from all states from `epsilonB`, returns all possible states
            epsilonFinal = eClose trans step                                        -- ECLOSE of step
        in          
            nfaRun trans xs epsilonFinal                                            -- calls itself with the rest of the input and all the possible end states after reading the current symbol
nfaRun trans [] st = eClose trans st


-- Calculates the ECLOSE of a set of states

eClose :: (Eq symbol, Eq state) => (state -> Transition symbol -> [state])          -- Transition function
                -> [state]                                                          -- Start states
                -> [state]                                                          -- End states
eClose trans st = 
        let 
            eclose = nub (concatMap (`trans` Epsilon) st)
        in
            if eclose == st then st else eClose trans eclose


-- Checks whether the given NFA accepts or rejects the given input.

nfaAccept :: (Eq symbol, Eq state) => NFA symbol state                              -- NFA
            -> [Transition symbol]                                                  -- Input
            -> Bool                                                                 -- Accept/Reject
nfaAccept (Nfa _ _ start final trans) input = 
    hasAny (nfaRun trans input [start]) final


nfaIntersect :: (Eq symbol, Eq state) => NFA symbol state -> NFA symbol state -> NFA symbol (state,state)
nfaIntersect (Nfa states1 symbols1 start1 final1 trans1) (Nfa states2 symbols2 start2 final2 trans2) = 
    Nfa states3 symbols1 (start1,start2) final3 trans3
        where
            states3        = cartProd states1 states2
            final3         = cartProd final1  final2
            trans3 (x,y) a = cartProd (trans1 x a) (trans2 y a)


nfaUnion :: (Eq symbol, Eq state) => NFA symbol state -> NFA symbol state -> NFA symbol (state,state)
nfaUnion (Nfa states1 symbols1 start1 final1 trans1) (Nfa states2 symbols2 start2 final2 trans2) = 
    Nfa states3 symbols1 (start1,start2) final3 trans3
        where
            states3        = cartProd states1 states2
            final3         = cartProd final1  states2 ++ cartProd states1 final2
            trans3 (x,y) a = cartProd (trans1 x a) (trans2 y a)