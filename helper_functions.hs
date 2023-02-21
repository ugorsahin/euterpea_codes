import Euterpea
import System.Random
import System.Random.Distributions


listmaker :: a -> (a,a) -> [[a]]
listmaker k (x,xs) = 

aanota :: Int -> Int -> [Int]
aanota kat num = take num $ randomRs (1,4) (mkStdGen kat) 

bigList :: [Int] -> [Int] -> [[Int]]
bigList (x:xs) (y:ys) = [take x (y:ys)] ++ bigList xs (ys++[y])
bigList _ a = []

timesn :: Int -> Music a -> Music a
timesn 0 m = rest 0
timesn n m = m :+: timesn (n-1) m

aynianda :: Dur -> [Int] -> Music Pitch
aynianda d (x:xs) = note d (pitch x) :=: aynianda d xs
aynianda d _ = rest 0

revM :: Music a -> Music a
revM n@(Prim _) = n
revM (Modify c m) = Modify c (revM m)
revM (m1 :+: m2) = revM m2 :+: revM m1
revM (m1 :=: m2) = if d1>d2 then revM m1 :=: (rest (d1-d2) :+: revM m2) else (rest (d2-d1) :+: revM m1) :=: revM m2
                   where
                    d1 = dur m1
                    d2 = dur m2

devamli :: [Int] -> [[Int]]
devamli (x:xs) = [x:xs] ++ devamli xs
devamli _ = []

ayniandadev :: Dur -> [[Int]] -> Music Pitch
ayniandadev d (x:xs) = aynianda d x :+: ayniandadev d xs
ayniandadev d _ = rest 0


linemaker d lst = ayniandadev d (devamli lst)

mel1 = timesn 4 (linemaker (1/32) [20..100] :+: (revM $ linemaker (1/32) [20..100]))
mel2 = instrument Cello $ timesn 1 (linemaker (1/4) [20,22..100] :+: (revM $ linemaker (1/4) [20,22..100]))




uzunluk :: [a] -> Int
uzunluk (x:xs) = 1 + uzunluk xs
uzunluk _ = 0
