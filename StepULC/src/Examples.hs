module Examples where

import ULC

intToChurch :: Int -> ULC
intToChurch = Lam . Lam . body where
    body i = iterate (App (Var 2)) (Var 1) !! i

listToChurch :: [ULC] -> ULC
listToChurch = Lam . Lam . body where
    body = foldr (\x l -> App (App (Var 2) x) l) (Var 1)

intListToChurch :: [Int] -> ULC
intListToChurch = listToChurch . map intToChurch

succE = Lam $ Lam $ Lam $ App (Var 2) $ foldl1 App [Var 3, Var 2, Var 1]
predE = Lam $ Lam $ Lam $ foldl1 App [Var 3, Lam $ Lam $ foldr1 App [Var 1, Var 2, Var 4], Lam $ Var 2, Lam $ Var 1]
fixE = Lam $ foldl1 App [Lam $ foldr1 App [Var 2, Var 1, Var 1], Lam $ foldr1 App [Var 2, Var 1, Var 1]]
trueE = Lam $ Lam $ Var 2
falseE = Lam $ Lam $ Var 1
isZeroE = Lam $ foldl1 App [Var 1, Lam falseE, trueE]
multE = Lam $ Lam $ Lam $ foldr1 App [Var 3, Var 2, Var 1]
facFixE = Lam $ Lam $ foldl1 App [App isZeroE (Var 1), Lam $ Var 1, foldr1 App [App multE (Var 1), Var 2, App predE (Var 1)]]
facE = App fixE facFixE
facN n = App facE $ intToChurch n
fac4 = facN 4
fac4Expected = intToChurch 24

nilE = falseE
consE = Lam $ Lam $ Lam $ Lam $ foldl1 App [Var 2, Var 4, foldl1 App [Var 3, Var 2, Var 1]]
pairE = Lam $ Lam $ Lam $ foldl1 App [Var 1, Var 3, Var 2]
fstE = Lam $ App (Var 1) trueE
sndE = Lam $ App (Var 1) falseE
rangeE = Lam $ App sndE $ foldl1 App [Var 1, nextRangeE, foldl1 App [pairE, Lam $ Lam $ App (Var 2) (Var 1), nilE]] where
    nextRangeE = Lam $ foldl1 App [pairE, foldr1 App [succE, fstE, Var 1], foldl1 App [consE, App fstE (Var 1), App sndE (Var 1)]]
range4 = App rangeE $ intToChurch 4
range4Expected = intListToChurch [4, 3, 2, 1]
