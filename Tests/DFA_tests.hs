module Tests.DFA_tests where


import Test.HUnit
import AutomataLibrary.DFA.DFA

-- DFA that accepts strings of 0s and 1s that end in the substring "010"

dfa1 :: DFA Char Int
dfa1 = Dfa [0,1,2,3,4] ['0','1'] 0 [3] delta
  where delta 0 '0' = 1
        delta 0 '1' = 0
        delta 1 '0' = 1
        delta 1 '1' = 2
        delta 2 '0' = 3
        delta 2 '1' = 0
        delta 3 '0' = 1
        delta 3 '1' = 2
        delta _ _   = 4 

dfaz :: DFA Char Int
dfaz = Dfa [0,1,2,3,4] ['0','1'] 0 [3] delta
  where delta 0 '0' = 1
        delta 1 '1' = 2
        delta 2 '0' = 3
        delta _ _   = 4

test1 = TestCase (assertBool "010 should be accepted" (dfaAccept dfa1 "010"))
test2 = TestCase (assertBool "1a01 should be rejected" (not $ dfaAccept dfa1 "1a010"))
test3 = TestCase (assertBool "011010 should be accepted" (dfaAccept dfa1 "011010"))

tests1 = TestList [TestLabel "test 010 - accept" test1, TestLabel "test 1a010 - reject" test2, TestLabel "test 011010 - accept" test3]

-- DFA that accepts strings of 0s and 1s that begins with 0 and ends with 1.

dfa2 :: DFA Int String
dfa2 = Dfa ["s0","s1","s2","d"] [0,1] "s0" ["s2"] delta
  where delta "s0" 0 = "s1"
        delta "s0" 1 = "d"
        delta "s1" 0 = "s1"
        delta "s1" 1 = "s2"
        delta "s2" 0 = "s1"
        delta "s2" 1 = "s2"
        delta "d"  0 = "d"
        delta "d"  1 = "d"
        delta _ _    = "d"

test4 = TestCase (assertBool "0101 should be accepted" (dfaAccept dfa2 [0,1,0,1]))
test5 = TestCase (assertBool "10011 should be rejected" (not $ dfaAccept dfa2 [1,0,0,1,1]))
test6 = TestCase (assertBool "00201 should be rejected" (not $ dfaAccept dfa2 [0,0,2,0,1]))

tests2 = TestList [TestLabel "test 0101 - accept" test4, TestLabel "test 10011 - reject" test5, TestLabel "test 00201 - reject" test6]

main = do
    runTestTT tests1
    runTestTT tests2