import Euterpea

timesn :: Int -> Music a -> Music a
timesn 0 m = rest 0
timesn n m = m :+: timesn (n-1) m

tlist :: [Dur] -> Music Pitch -> Music Pitch
tlist (d:ds) mp = takor d mp :+: tlist ds mp
tlist [] _ = rest 0

takor :: Dur -> Music Pitch -> Music Pitch
takor yd (Prim (Note d p)) = note yd p
takor yd (Prim (Rest d)) = rest d
takor yd (m1 :+: m2) = takor yd m1 :+: takor yd m2
takor yd (m1 :=: m2) = takor yd m1 :=: takor yd m2
takor yd (Modify (Tempo r) m) = tempo r (takor yd m)
takor yd (Modify c m) = Modify c (takor yd m)

notakor :: Pitch -> [Int] -> Music Pitch
notakor p (i:is) = note 0 p :=: notakor (trans i p) is
notakor p [] = note 0 p

 
melmake :: Pitch -> [Dur] -> [Int] -> Music Pitch
melmake p (d:ds) (x:xs)
    | x == -100 = rest d :+: melmake p ds xs
    | otherwise = note d p :+: melmake (trans x p) ds xs
melmake p (d:ds) _ = note d p

mel1 = instrument SlapBass1 $ timesn 10 $ melmake (A,1) [(1/3),(1/6),(1/3),(1/6),(1/6),(1/6),(1/3),(1/3),(1/3),(1/3)] [8,4,-1,-1,-3,3,-5,3,-1]
mel2 = timesn 10 $ firstc :+: secc

zamanakor = [(4/9),(5/18),(2/9),(2/9)]

stc = notakor (A,3) [7,6]
firstc = tlist zamanakor stc

ecc = notakor (A,3) [7,5]
secc = tlist zamanakor ecc
