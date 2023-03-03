module Tests.NFA_tests where


import Test.HUnit
import AutomataLibrary.NFA.NFA
import AutomataLibrary.Misc.Data_types




-- NFA that accepts words comprised of 0s and 1s that end with a 1.

nfa1 :: NFA Integer String
nfa1 = Nfa ["s0","s1","s2"] [Symbol 0, Symbol 1] "s0" ["s2"] delta
    where
        delta "s0" (Symbol 0) = ["s1"]
        delta "s0" (Symbol 1) = []
        delta "s0" Epsilon    = ["s0","s1"]
        delta "s1" (Symbol 0) = ["s1"]
        delta "s1" (Symbol 1) = ["s1","s2"]
        delta "s1" Epsilon    = ["s1"]
        delta "s2" (Symbol 0) = []
        delta "s2" (Symbol 1) = []
        delta "s2" Epsilon    = ["s2"]

test1 = TestCase (assertBool "010101 should be accepted" (nfaAccept nfa1 [Symbol 0, Symbol 1, Symbol 0, Symbol 1, Symbol 0, Symbol 1]))
test2 = TestCase (assertBool "00000 should be rejected" (not $ nfaAccept nfa1 [Symbol 0, Symbol 0, Symbol 0, Symbol 0, Symbol 0]))
test3 = TestCase (assertBool "11110 should be rejected" (not $ nfaAccept nfa1 [Symbol 1, Symbol 1, Symbol 1, Symbol 1, Symbol 0]))

tests1 = TestList [TestLabel "test 010101 - accept" test1, TestLabel "test 00000 - reject" test2, TestLabel "test 11110 - reject" test3]


main = do
    runTestTT tests1