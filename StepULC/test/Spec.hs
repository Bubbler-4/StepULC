import ULC
import Examples
import Test.HUnit

main :: IO ()
main = do
    putStrLn "Testing semantic1"
    runTestTT testSem1
    putStrLn "Testing semantic1_1"
    runTestTT testSem1_1
    putStrLn "Testing semantic1_2"
    runTestTT testSem1_2
    putStrLn "Testing semantic2"
    runTestTT testSem2
    putStrLn "Testing semantic2_1"
    runTestTT testSem2_1
    putStrLn "Testing semantic2_2"
    runTestTT testSem2_2
    putStrLn "Testing semantic3"
    runTestTT testSem3
    return ()

e1 = App (intToChurch 2) (intToChurch 2)
e1Expected = intToChurch 4
e2 = Lam $ Lam $ App (Lam $ App (Var 1) (Lam $ Var 2)) (Lam $ App (App (Var 2) (Var 1)) (Var 3))
e2Expected = Lam $ Lam $ App (App (Var 1) (Lam $ Lam $ App (App (Var 3) (Var 1)) (Var 4))) (Var 2)
e3 = Lam $ Lam $ App (Lam $ App (Var 1) (Lam $ Var 2)) (Lam $ App (App (Var 2) (Lam $ Var 2)) (Var 3))
e3Expected = Lam $ Lam $ App (App (Var 1) (Lam $ Lam $ Lam $ App (App (Var 4) (Lam $ Var 2)) (Var 5))) (Var 2)

-- semantic1
testSem1 = TestList [TestLabel "sem1_1" sem1_1, TestLabel "sem1_2" sem1_2, TestLabel "sem1_3" sem1_3, TestLabel "sem1_4" sem1_4, TestLabel "sem1_5" sem1_5] where
    sem1_1 = TestCase $ do
        assertEqual "2 2 = 4" e1Expected (semantic1 e1)
    sem1_2 = TestCase $ do
        assertEqual "simpler" e2Expected (semantic1 e2)
    sem1_3 = TestCase $ do
        assertEqual "less simpler" e3Expected (semantic1 e3)
    sem1_4 = TestCase $ do
        assertEqual "fac 4 = 24" fac4Expected (semantic1 fac4)
    sem1_5 = TestCase $ do
        assertEqual "range 4 = [4, 3, 2, 1]" range4Expected (semantic1 range4)

testSemGeneric f testList = TestCase $ do
    mapM_ (\(name, e, expected) -> assertEqual name expected (f e)) testList

testList =
    [ ("no reduction", e3Expected, e3Expected)
    , ("no reduction 2", Var 3, Var 3)
    , ("simpler", e2, e2Expected)
    , ("less simpler", e3, e3Expected)
    , ("2 2 = 4", e1, e1Expected)
    , ("fac 4 = 24", fac4, fac4Expected)
    , ("range 4 = [4, 3, 2, 1]", range4, range4Expected)
    ]

-- semantic1-1
testSem1_1 = testSemGeneric semantic1_1 testList

-- semantic1-2
testSem1_2 = testSemGeneric semantic1_2 testList

-- semantic1-3
testSem1_3 = testSemGeneric semantic1_3 testList

-- semantic2
testSem2 = testSemGeneric semantic2 testList

-- semantic2-1
testSem2_1 = testSemGeneric semantic2_1 testList

-- semantic2-2
testSem2_2 = testSemGeneric semantic2_2 testList

-- semantic3
testSem3 = testSemGeneric semantic3 testList
