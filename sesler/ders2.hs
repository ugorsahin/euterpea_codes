import Euterpea

--bassolcu :: Integer -> [Integer] -> [Dur] -> Music Pitch
--bassolcu bnote (x:xs) (y:ys) = 


--bnota :: Pitch ->   


melodymaker :: [Int] -> [Dur] -> Music Pitch
melodymaker (x:xs) (y:ys) = note y (pitch (x+30)) :+: melodymaker xs ys
melodymaker [] _ = rest 0

yukselir :: Music Pitch -> Music Pitch
yukselir (Prim (Note d p)) = let nota = absPitch p 
								in melodymaker [nota,nota+2,nota+4] [d,d,d]
yukselir (Prim (Rest d)) = rest d
yukselir (m1 :+: m2) = yukselir m1 :+: yukselir m2
yukselir (m1 :=: m2) = yukselir m1 :=: yukselir m2
yukselir (Modify (Tempo r) m) = tempo r (yukselir m)
yukselir (Modify c m) = Modify c (yukselir m)


perkus :: Music Pitch -> Music Pitch
perkus a = instrument Percussion a



deneme :: Music Pitch
deneme = (note (1/4) (A,4) :+: note en (C,4)) :=: note (1) (F,6)


gamyapici :: [Int] -> Pitch -> Music Pitch
gamyapici (x:xs) p = note qn p :+: gamyapici xs (trans x p)


deneme3 :: [Integer] -> [Integer]
deneme3 (y:yp) = [y+2] : deneme3 yp
deneme3 [] = []
























