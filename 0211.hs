import Euterpea


majorakor :: Dur -> Pitch -> Music Pitch
majorakor d p = note d p :=: note d (trans 4 p) :=: note d (trans 7 p)


majorList :: [Dur] -> [Pitch] -> Music Pitch
majorList (d:ds) (x:xs) = majorakor d x :+: majorList ds xs
majorList _ _ = rest 0

melmake :: [Dur] -> [Int] -> Music Pitch
melmake (d:ds) (x:xs)
    | x == 0   = rest d :+: melmake ds xs
    | otherwise = note d (pitch x) :+: melmake ds xs
melmake _ _ = rest 0


melmake2 :: Pitch -> [Dur] -> [Int] -> Music Pitch
melmake2 p (d:ds) (x:xs)
    | x == -100 = rest d :+: melmake2 p ds xs
    | otherwise = note d p :+: melmake2 (trans x p) ds xs
melmake2 p _ _ = rest 0


mel = majorList [wn,wn,wn,wn,wn] [(C,3),(G,3),(F,3),(F,3),(G,3)]
mel2 = melmake2 (C,4) [qn,qn,wn,qn,wn,hn,qn,hn,qn,qn,qn,qn] [2,2,1,-5,4,1,2,2,-5,1,4,2]