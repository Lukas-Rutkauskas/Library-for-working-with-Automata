module AutomataLibrary.DFA.DFA where

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
