module Source.Source where


import Data.List
import Data.Maybe
import Data.Char

-- Data type for epsilon transitions and normal transitions | isomorphic to Maybe
data Transition a = Epsilon | Symbol a
    deriving (Show,Eq)

data Tree a = Nil | Node a (Tree a) (Tree a)
    deriving (Show,Eq)

data Move = L | R
    deriving (Show,Eq)

hasAny [] _          = False
hasAny _ []          = False
hasAny search (x:xs) = if x `elem` search then True else hasAny search xs


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

dfaRun ::  state                                            -- Start state
        -> [symbol]                                         -- Input
        -> (state -> symbol -> state)                       -- Transition function
        -> state                                            -- End state
dfaRun st input trans = foldl trans st input


-- Checks whether the given DFA accepts or rejects the given input.

dfaAccept :: Eq state => DFA symbol state                   -- DFA
            -> [symbol]                                     -- Input
            -> Bool                                         -- Accept/Reject
dfaAccept (Dfa _ _ start final trans) input = 
    dfaRun start input trans `elem` final


-- Data type of a Non-Deterministic Finite Automata polymorphically parameterized
-- with the type parameters 'symbol' and 'state', for the type of symbols and states respectively.

data NFA symbol state = Nfa
                        [state]                                         -- Finite set of states
                        [Transition symbol]                             -- Finite set of symbols (alphabet)
                        state                                           -- Starting state
                        [state]                                         -- Finite set of final/accepting states
                        (state -> Transition symbol -> [state])         -- Transition function


-- Executes the run of an NFA on the input word by starting at the start state(s)
-- and taking transitions according to the transition function.
-- Return the end states when the input has been fully expended.

nfaRun :: Eq state => (state -> Transition symbol -> [state])           -- Transition function
        -> [Transition symbol]                                          -- Input
        -> [state]                                                      -- Start states
        -> [state]                                                      -- End states
nfaRun trans (x:xs) st = 
        let 
            epsilonB     = eClose trans st                              -- ECLOSE of starting states
            step         = nub (concatMap (`trans` x) epsilonB)         -- transition upon reading an input symbol, starting from all states from `epsilonB`, returns all possible states
            epsilonFinal = eClose trans step                            -- ECLOSE of step
        in
            nfaRun trans xs epsilonFinal                                -- calls itself with the rest of the input and all the possible end states after reading the current symbol
nfaRun trans [] st = eClose trans st


-- Calculates the ECLOSE of a set of states

eClose :: Eq state => (state -> Transition symbol -> [state])           -- Transition function
                -> [state]                                              -- Start states
                -> [state]                                              -- End states
eClose trans st = 
        let 
            eclose = nub (concatMap (`trans` Epsilon) st)
        in
            if eclose == st then st else eClose trans eclose


-- Checks whether the given NFA accepts or rejects the given input.

nfaAccept :: Eq state => NFA symbol state                               -- NFA
            -> [Transition symbol]                                      -- Input
            -> Bool                                                     -- Accept/Reject
nfaAccept (Nfa _ _ start final trans) input = 
    hasAny (nfaRun trans input [start]) final

-- Thompson's Construction

-- regexToNFA :: String -> NFA symbol state
-- regexToNFA regex = 

createMappings :: [String] -> [String] -> (String -> (Int, String),(Int, String) -> String)
createMappings s1 s2 = 
    let
        nameList = ['\128'..]
        m x
            | length s1 >  no                                   = (1, s1 !! no)                      -- States of the 1st NFA
            | length s1 <= no && no < (length s1 + length s2)   = (2, s2 !! (no - length s1))        -- States of the 2nd NFA
            | otherwise                                         = (0, "")                            -- error
            where 
                no = fromJust $ elemIndex (head x) nameList
        m' (x, s)
            | x == 1    =                                                                           -- States of 1st NFA
                let
                    no1 = fromJust $ elemIndex s s1
                in
                    drop no1 $ take (no1+1) nameList   

            | x == 2    =                                                                           -- States of the 2nd NFA
                let
                    no2 = fromJust $ elemIndex s s2
                in
                    drop (no2 + length s1) $ take (no2 + length s1 + 1) nameList

            | otherwise = ""                                                                        -- error
    in
        (m, m')

convertList :: [String] -> ((Int, String) -> String) -> Int -> [String]
convertList oglist mapping no = 
    let
        pairs   = map (no, ) oglist
        newlist = map mapping pairs
    in
        newlist

symbolToNFA :: String -> NFA String String
symbolToNFA regex =
        let
            states  = ["s0","s1"]
            symbols = [Symbol regex]
            start   = "s0"
            final   = ["s1"]
            trans "s0" symbol
                | symbol == Symbol regex = ["s1"]
                | symbol == Epsilon      = ["s0"]
            trans "s1" symbol
                | symbol == Symbol regex = []
                | symbol == Epsilon      = ["s1"]
            trans _ _                   = []
        in
            Nfa states symbols start final trans


orNFA :: Eq a => NFA a String -> NFA a String -> NFA a String
orNFA (Nfa states1 symbols1 start1 final1 trans1) (Nfa states2 symbols2 start2 final2 trans2) = 
    let
        (m, m')    = createMappings states1 states2
        nameList = ['\128'..]

        newStates1 = convertList states1 m' 1
        newStates2 = convertList states2 m' 2
        newStart1  = m' (1,start1)
        newStart2  = m' (2,start2)
        newFinal1  = convertList final1  m' 1
        newFinal2  = convertList final2  m' 2
        no         = length states1 + length states2
        addStart   = drop no $ take (no+1) nameList

        transNew st sm
            | st == addStart && sm == Epsilon                           = newStart1 : [newStart2]
            
            | st `elem` newStates1 && sm `elem` symbols1 ++ [Epsilon]   = 
                let
                    (no, state) = m st
                    result = convertList (trans1 state sm) m' 1
                in result

            | st `elem` newStates2 && sm `elem` symbols2 ++ [Epsilon]   = 
                let
                    (no, state) = m st
                    result = convertList (trans2 state sm) m' 2
                in result

            | otherwise                                             = []

    in
        Nfa (newStates1 ++ newStates2 ++ [addStart]) (symbols1 ++ symbols2) addStart (newFinal1 ++ newFinal2) transNew


andNFA :: Eq a => NFA a String -> NFA a String -> NFA a String
andNFA (Nfa states1 symbols1 start1 final1 trans1) (Nfa states2 symbols2 start2 final2 trans2) = 
    let
        (m, m')    = createMappings states1 states2

        newStates1 = convertList states1 m' 1
        newStates2 = convertList states2 m' 2
        newStart1  = m' (1,start1)
        newStart2  = m' (2,start2)
        newFinal1  = convertList final1  m' 1
        newFinal2  = convertList final2  m' 2

        transNew st sm
            | st `elem` newFinal1  && sm == Epsilon                    = 
                let
                   (no, state) = m st
                   result = convertList (trans1 state sm) m' 1
                in result ++ [newStart2]

            | st `elem` newStates1 && sm `elem` symbols1 ++ [Epsilon]  = 
                let
                    (no, state) = m st
                    result = convertList (trans1 state sm) m' 1
                in result

            | st `elem` newStates2 && sm `elem` symbols2 ++ [Epsilon]  = 
                let
                    (no, state) = m st
                    result = convertList (trans2 state sm) m' 2
                in result

            | otherwise                                             = []
    in
        Nfa (newStates1 ++ newStates2) (symbols1 ++ symbols2) newStart1 newFinal2 transNew


kleeneNFA :: Eq a => NFA a String -> NFA a String
kleeneNFA (Nfa states symbols start final trans) = 
    let
        (m, m')    = createMappings states []
        nameList = ['\128'..]

        newStates = convertList states m' 1
        newStart  = m' (1,start)
        newFinal  = convertList final  m' 1
        no         = length states
        addStart   = drop no $ take (no+1) nameList

        transNew st sm
            | st == addStart && sm == Epsilon                           = [newStart]
            
            | st `elem` newFinal && sm == Epsilon   = 
                let
                    (no, state) = m st
                    result = convertList (trans state sm) m' 1
                in result ++ [newStart]

            | st `elem` newStates && sm `elem` symbols ++ [Epsilon]   = 
                let
                    (no, state) = m st
                    result = convertList (trans state sm) m' 1
                in result

            | otherwise                                             = []

    in
        Nfa (newStates ++ [addStart]) symbols addStart newFinal transNew


-- checkRegex :: String -> String -> Bool
-- checkRegex regex input = 


regexToNFA :: String -> NFA String String -> NFA String String
regexToNFA [] curr = curr
regexToNFA (x:xs) curr
    | x == '+'  = regexToNFA (drop (rCount xs) xs) (curr `orNFA`  regexHelp xs)
    | x == '.'  = regexToNFA (drop (rCount xs) xs) (curr `andNFA` regexHelp xs)
   -- | otherwise = regexToNFA (drop (rCount xs) xs) (              regexHelp xs)


regexHelp :: String -> NFA String String
regexHelp (x:xs)
    | null xs           = symbolToNFA [x]
    | head xs == '*'    = kleeneNFA (symbolToNFA [x])
    | otherwise         = symbolToNFA [x]

rCount :: String -> Int
rCount (x:xs)
    | null xs           = 1
    | head xs == '*'    = 2
    | otherwise         = 1

parseRegex :: String -> String -> [String]
parseRegex (x:xs) s
    | x == '('  = s : parseRegex xs ""
    | x == ')'  =
        case () of
            ()  | null xs                           -> [s]
                | head xs == '*' && null (tail xs)  -> [s++")*"]
                | otherwise                         ->  (s++")*"++[head (tail xs)]) : parseRegex (tail $ tail xs) ""

    | otherwise = parseRegex xs (s ++ [x])

parseRegex [] s = [s]


treeToNFA :: Tree String -> NFA String String
treeToNFA (Node x y z)
    | x == "+"  = orNFA      (treeToNFA y) (treeToNFA z)
    | x == "."  = andNFA     (treeToNFA y) (treeToNFA z)
    | x == "*"  = kleeneNFA  (treeToNFA y)
    | otherwise = symbolToNFA x


-- regexToTree :: String -> Tree String
-- regexToTree regex =
--     let
--         (op,reg) = readUntil regex ".+*"
--         leftRegex = drop (length reg) regex
--         result = case [op] of
--             "*"         -> Node "*" (Node reg Nil Nil) Nil
--             _   ->
--                 let 
--                     (op2,reg2) = readUntil leftRegex ".+*"
--                 in
--                     Node [op] (Node reg Nil Nil) (Node reg2 Nil Nil)

--     in

readUntil :: String -> String -> (Char,String)
readUntil (x:xs) symbols
    | x `elem` symbols  = (x,"")
    | otherwise         = (\(x,y) f -> (x,f y)) (readUntil xs symbols) ([x] ++)

--                      [Transition symbol]                                                                     -- Finite set of symbols (stack alphabet)
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


    
data TM symbol state = Tm 
                        [state]                                     -- set of states
                        [symbol]                                    -- input alphabet
                        state                                       -- starting state
                        state                                       -- accept state
                        state                                       -- reject state
                        ((state,symbol) -> (state,symbol,Move))     -- transition function
                        ([symbol],[symbol])                         -- tape
    

-- IDEA
-- Store the configuration of TM as (state,tape+head position)
-- tape + head position as ([state],[state]) which is tape contents to the left and right of the head position.
-- `tmRun` returns tm with its configuration. If tm enters accept or reject state it halts.
-- `tmAccept` would just check if the current state in the configuration is an accept state or reject state.


-- tmRun :: (Eq a, Eq b) => [a] -> TM a b -> TM a b
-- tmRun input (Tm st abc start accept reject trans (l,r)) = tmRun' (Tm st abc start accept reject trans ())

-- tmRun' :: (Eq a, Eq b) => [a] -> TM a b -> TM a b
-- tmRun' input a = 


-- TESTS

tree1 = Node "+" (Node "." (Node "a" Nil Nil) (Node "b" Nil Nil)) (Node "." (Node "c" Nil Nil) (Node "*" (Node "d" Nil Nil) Nil))

pdaSt = ["s1","s2","s3","s4"]
pdaSm = [Symbol "0", Symbol "1"]
pdaStSt = "s1"
pdaFSt  = ["s4"]
pdaTr (Symbol "0") Epsilon "s1"     = []
pdaTr (Symbol "0") (Symbol "0") "s1"  = []
pdaTr (Symbol "0") (Symbol "1") "s1"  = []
pdaTr (Symbol "0") (Symbol "$") "s1"  = []
pdaTr (Symbol "1") Epsilon "s1"     = []
pdaTr (Symbol "1") (Symbol "0") "s1"  = []
pdaTr (Symbol "1") (Symbol "1") "s1"  = []
pdaTr (Symbol "1") (Symbol "$") "s1"  = []
pdaTr Epsilon Epsilon "s1"          = [("s2",Symbol "$")]
pdaTr Epsilon (Symbol "0") "s1"       = []
pdaTr Epsilon (Symbol "1") "s1"       = []
pdaTr Epsilon (Symbol "$") "s1"       = []

pdaTr (Symbol "0") Epsilon "s2"     = [("s2",Symbol "0")]
pdaTr (Symbol "0") (Symbol "0") "s2"  = []
pdaTr (Symbol "0") (Symbol "1") "s2"  = []
pdaTr (Symbol "0") (Symbol "$") "s2"  = []
pdaTr (Symbol "1") Epsilon "s2"     = []
pdaTr (Symbol "1") (Symbol "0") "s2"  = [("s3",Epsilon)]
pdaTr (Symbol "1") (Symbol "1") "s2"  = []
pdaTr (Symbol "1") (Symbol "$") "s2"  = []
pdaTr Epsilon Epsilon "s2"          = []
pdaTr Epsilon (Symbol "0") "s2"       = []
pdaTr Epsilon (Symbol "1") "s2"       = []
pdaTr Epsilon (Symbol "$") "s2"       = []

pdaTr (Symbol "0") Epsilon "s3"     = []
pdaTr (Symbol "0") (Symbol "0") "s3"  = []
pdaTr (Symbol "0") (Symbol "1") "s3"  = []
pdaTr (Symbol "0") (Symbol "$") "s3"  = []
pdaTr (Symbol "1") Epsilon "s3"     = []
pdaTr (Symbol "1") (Symbol "0") "s3"  = [("s3",Epsilon)]
pdaTr (Symbol "1") (Symbol "1") "s3"  = []
pdaTr (Symbol "1") (Symbol "$") "s3"  = []
pdaTr Epsilon Epsilon "s3"          = []
pdaTr Epsilon (Symbol "0") "s3"       = []
pdaTr Epsilon (Symbol "1") "s3"       = []
pdaTr Epsilon (Symbol "$") "s3"       = [("s4",Epsilon)]

pdaTr (Symbol "0") Epsilon "s4"     = []
pdaTr (Symbol "0") (Symbol "0") "s4"  = []
pdaTr (Symbol "0") (Symbol "1") "s4"  = []
pdaTr (Symbol "0") (Symbol "$") "s4"  = []
pdaTr (Symbol "1") Epsilon "s4"     = []
pdaTr (Symbol "1") (Symbol "0") "s4"  = []
pdaTr (Symbol "1") (Symbol "1") "s4"  = []
pdaTr (Symbol "1") (Symbol "$") "s4"  = []
pdaTr Epsilon Epsilon "s4"          = []
pdaTr Epsilon (Symbol "0") "s4"       = []
pdaTr Epsilon (Symbol "1") "s4"       = []
pdaTr Epsilon (Symbol "$") "s4"       = []

pdaTest = Pda pdaSt pdaSm pdaStSt pdaFSt pdaTr

setOfStatesB  = ["s2","s3"]
setOfSymbolsB = [Symbol "b"]
startStateB   = "s2"
finalStatesB  = ["s3"]
transitionFB  "s2" (Symbol "b") = ["s3"]
transitionFB  "s2" Epsilon      = ["s2"]
transitionFB  "s3" (Symbol "b") = []
transitionFB  "s3" Epsilon      = ["s3"]
transitionFB _ _ = []

testNFAB = Nfa setOfStatesB setOfSymbolsB startStateB finalStatesB transitionFB

setOfStatesC  = ["s2","s3"]
setOfSymbolsC = [Symbol "c"]
startStateC   = "s2"
finalStatesC  = ["s3"]
transitionFC  "s2" (Symbol "c") = ["s3"]
transitionFC  "s2" Epsilon      = ["s2"]
transitionFC  "s3" (Symbol "c") = []
transitionFC  "s3" Epsilon      = ["s3"]
transitionFC _ _ = []

testNFAC = Nfa setOfStatesC setOfSymbolsC startStateC finalStatesC transitionFC

setOfStatesNew  = ["s0","s1","s2","s3"]
setOfSymbolsNew = [Symbol "a", Symbol "b"]
startStateNew   = "s0"
finalStatesNew  = ["s3"]
transitionFNew  "s0" (Symbol "a") = ["s1"]
transitionFNew  "s0" Epsilon      = ["s0"]
transitionFNew  "s1" (Symbol "a") = []
transitionFNew  "s1" Epsilon      = ["s1", "s2"]
transitionFNew  "s2" (Symbol "b") = ["s3"]
transitionFNew  "s2" Epsilon      = ["s2"]
transitionFNew  "s3" (Symbol "b") = []
transitionFNew  "s3" Epsilon      = ["s3"]


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