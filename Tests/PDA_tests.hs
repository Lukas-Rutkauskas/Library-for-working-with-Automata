module Tests.PDA_tests where

import Test.HUnit
import AutomataLibrary.PDA.PDA
import AutomataLibrary.Misc.Data_types


pda1 :: PDA String String
pda1 = Pda ["s1","s2","s3","s4"] [Symbol "0", Symbol "1"] "s1" ["s4"] delta
    where
        delta (Symbol "0") Epsilon "s1"     = []
        delta (Symbol "0") (Symbol "0") "s1"  = []
        delta (Symbol "0") (Symbol "1") "s1"  = []
        delta (Symbol "0") (Symbol "$") "s1"  = []
        delta (Symbol "1") Epsilon "s1"     = []
        delta (Symbol "1") (Symbol "0") "s1"  = []
        delta (Symbol "1") (Symbol "1") "s1"  = []
        delta (Symbol "1") (Symbol "$") "s1"  = []
        delta Epsilon Epsilon "s1"          = [("s2",Symbol "$")]
        delta Epsilon (Symbol "0") "s1"       = []
        delta Epsilon (Symbol "1") "s1"       = []
        delta Epsilon (Symbol "$") "s1"       = []

        delta (Symbol "0") Epsilon "s2"     = [("s2",Symbol "0")]
        delta (Symbol "0") (Symbol "0") "s2"  = []
        delta (Symbol "0") (Symbol "1") "s2"  = []
        delta (Symbol "0") (Symbol "$") "s2"  = []
        delta (Symbol "1") Epsilon "s2"     = []
        delta (Symbol "1") (Symbol "0") "s2"  = [("s3",Epsilon)]
        delta (Symbol "1") (Symbol "1") "s2"  = []
        delta (Symbol "1") (Symbol "$") "s2"  = []
        delta Epsilon Epsilon "s2"          = []
        delta Epsilon (Symbol "0") "s2"       = []
        delta Epsilon (Symbol "1") "s2"       = []
        delta Epsilon (Symbol "$") "s2"       = []

        delta (Symbol "0") Epsilon "s3"     = []
        delta (Symbol "0") (Symbol "0") "s3"  = []
        delta (Symbol "0") (Symbol "1") "s3"  = []
        delta (Symbol "0") (Symbol "$") "s3"  = []
        delta (Symbol "1") Epsilon "s3"     = []
        delta (Symbol "1") (Symbol "0") "s3"  = [("s3",Epsilon)]
        delta (Symbol "1") (Symbol "1") "s3"  = []
        delta (Symbol "1") (Symbol "$") "s3"  = []
        delta Epsilon Epsilon "s3"          = []
        delta Epsilon (Symbol "0") "s3"       = []
        delta Epsilon (Symbol "1") "s3"       = []
        delta Epsilon (Symbol "$") "s3"       = [("s4",Epsilon)]

        delta (Symbol "0") Epsilon "s4"     = []
        delta (Symbol "0") (Symbol "0") "s4"  = []
        delta (Symbol "0") (Symbol "1") "s4"  = []
        delta (Symbol "0") (Symbol "$") "s4"  = []
        delta (Symbol "1") Epsilon "s4"     = []
        delta (Symbol "1") (Symbol "0") "s4"  = []
        delta (Symbol "1") (Symbol "1") "s4"  = []
        delta (Symbol "1") (Symbol "$") "s4"  = []
        delta Epsilon Epsilon "s4"          = []
        delta Epsilon (Symbol "0") "s4"       = []
        delta Epsilon (Symbol "1") "s4"       = []
        delta Epsilon (Symbol "$") "s4"       = []

test1 = TestCase (assertBool "011110 should be accepted" (pdaAccept pda1 [Symbol "0", Symbol "1", Symbol "1", Symbol "1", Symbol "1", Symbol "0"]))
test2 = TestCase (assertBool "101 should be rejected" (not $ pdaAccept pda1 [Symbol "1", Symbol "0", Symbol "1"]))
test3 = TestCase (assertBool "01 should be accepted" (pdaAccept pda1 [Symbol "0", Symbol "1"]))

tests1 = TestList [TestLabel "test 011110 - accept" test1, TestLabel "test 101 - reject" test2, TestLabel "test 01 - accept" test3]

main = do
    runTestTT tests1
