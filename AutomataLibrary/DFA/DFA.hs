module AutomataLibrary.DFA.DFA where


import AutomataLibrary.Misc.Helper_functions
import Data.List

-- Data type of a Deterministic Finite Automata polymorphically parameterized
-- with the type parameters 'symbol' and 'state', for the type of symbols and states respectively.

data DFA symbol state = Dfa
                        [state]                             -- Finite set of states
                        [symbol]                            -- Finite set of symbols (alphabet)
                        state                               -- Starting state
                        [state]                             -- Finite set of final/accepting states
                        (state -> symbol -> state)          -- Transition function


-- Executes the run of a DFA on the input word by starting at the start state
-- and taking transitions according to the transition function.
-- Return the end state when the input has been fully expended.

dfaRun :: (Eq symbol, Eq state) => state                    -- Start state
        -> [symbol]                                         -- Input
        -> (state -> symbol -> state)                       -- Transition function
        -> state                                            -- End state
dfaRun st input trans = foldl trans st input


-- Checks whether the given DFA accepts or rejects the given input.

dfaAccept :: (Eq symbol, Eq state) => DFA symbol state      -- DFA
            -> [symbol]                                     -- Input
            -> Bool                                         -- Accept/Reject
dfaAccept (Dfa _ _ start final trans) input = 
    dfaRun start input trans `elem` final


dfaIntersect :: (Eq symbol, Eq state) => DFA symbol state -> DFA symbol state -> DFA symbol (state,state)
dfaIntersect (Dfa states1 symbols1 start1 final1 trans1) (Dfa states2 symbols2 start2 final2 trans2) = 
    Dfa states3 symbols1 (start1,start2) final3 trans3
        where
            states3        = cartProd states1 states2
            final3         = cartProd final1  final2
            trans3 (x,y) a = (trans1 x a, trans2 y a)
            

dfaUnion :: (Eq symbol, Eq state) => DFA symbol state -> DFA symbol state -> DFA symbol (state,state)
dfaUnion (Dfa states1 symbols1 start1 final1 trans1) (Dfa states2 symbols2 start2 final2 trans2) = 
    Dfa states3 symbols1 (start1,start2) final3 trans3
        where
            states3        = cartProd states1 states2
            final3         = cartProd final1  states2 ++ cartProd states1 final2
            trans3 (x,y) a = (trans1 x a, trans2 y a)


dfaComplement :: (Eq symbol, Eq state) => DFA symbol state -> DFA symbol state
dfaComplement (Dfa states symbols start final trans) =
    Dfa states symbols start (states \\ final) trans


dfaSetDiff :: (Eq symbol, Eq state) => DFA symbol state -> DFA symbol state -> DFA symbol (state,state)
dfaSetDiff dfa1 dfa2 = dfa1 `dfaIntersect` dfaComplement dfa2