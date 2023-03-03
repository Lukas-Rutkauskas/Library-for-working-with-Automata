module AutomataLibrary.Regex.Regex where


import Text.Megaparsec
import Text.Parsec.Char
import AutomataLibrary.NFA.NFA
import AutomataLibrary.Misc.Data_types
import Data.List
import Data.Maybe
import Data.Void

type Parser = Parsec Void String

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


parseRegex :: String -> Maybe (NFA String String)
parseRegex str = parseMaybe regexParser str

regexParser :: Parser (NFA String String)
regexParser = choice $ map try [ parseSeq, parseStar, parseOr, oneChar ]

parseSeq :: Parser (NFA String String)
parseSeq = do
  lhs <- regexParser
  rhs <- regexParser
  return $ andNFA lhs rhs

parseStar :: Parser (NFA String String)
parseStar = do
  lhs <- regexParser
  string "*"
  return $ kleeneNFA lhs

parseOr :: Parser (NFA String String)
parseOr = do
  lhs <- regexParser
  string "|"
  rhs <- regexParser
  return $ orNFA lhs rhs

oneChar :: Parser (NFA String String)
oneChar = Text.Megaparsec.satisfy (/= '|' '*')
