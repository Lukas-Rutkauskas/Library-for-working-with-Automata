module Tests.TM_tests where


import Test.HUnit
import AutomataLibrary.TM.TM
import AutomataLibrary.Misc.Data_types


abcTM :: String -> TM Char String
abcTM [] = Tm ["q0"] [' '] "q0" "q0" "" delta "q0" ([],[]) ' '
    where
        delta _ = ("q0", ' ', S)
abcTM input = 
    Tm states symbols "q0" "q5" "qr" delta "q0" ([], drop 1 input ++ " ") (head input)
        where
            states = ["q0", "q1", "q2", "q3", "q4", "q5", "qr"]
            symbols = ['a', 'b', 'c', ' ', 'x', 'y', 'z']
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


test1 = TestCase (assertBool "aaaabbbbcccc should be accepted" (tmAccept $ abcTM "aaaabbbbcccc"))
test2 = TestCase (assertBool "aabbccc should be rejected" (not $ tmAccept $ abcTM "aabbccc"))
test3 = TestCase (assertBool "abc should be accepted" (tmAccept $ abcTM "abc"))
test4 = TestCase (assertBool "empty input should be accepted" (tmAccept $ abcTM ""))

tests1 = TestList [TestLabel "test aaaabbbbcccc - accept" test1, TestLabel "test aabbccc - reject" test2, TestLabel "test abc - accept" test3, TestLabel "test empty input - accept" test4]

main = do
    runTestTT tests1
