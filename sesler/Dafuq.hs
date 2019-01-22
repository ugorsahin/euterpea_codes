
{-# LANGUAGE Arrows #-}
import Euterpea
import Control.Arrow((>>>),(<<<),arr)


constA :: Clock c => Double -> SigFun c () Double
constA ph = proc () -> do
	s <- oscFixed ph -< ()
	outA -< s

s1 :: Clock c => SigFun c () Double
s1 = proc () -> do
    s <- oscFixed 440 -< ()
    outA -< s

tab1 :: Table
tab1 = tableSinesN 4096 [1]

tab2 :: Table
tab2 = tableSinesN 4096 [1,0.5,0.33]

s2 :: Clock c => SigFun c () Double
s2 = proc () -> do
    osc tab1 0 -< 440

s3 :: Clock c => SigFun c () Double
s3 = proc () -> do
    osc tab2 0 -< 440

s4 :: Clock c => SigFun c () Double
s4 = proc () -> do
    f0 <- oscFixed 440 -< ()
    f1 <- oscFixed 880 -< ()
    f2 <- oscFixed 1320 -< ()
    outA -< (f0 + 0.5*f1 + 0.33*f2) / 1.83

vibrato :: Clock c => Double -> Double -> SigFun c Double Double
vibrato vfrq dep = proc afrq -> do
    vib <- osc tab1 0 -< vfrq
    aud <- osc tab2 0 -< afrq + vib * dep
    outA -< aud

simpleClip :: Clock c => SigFun c Double Double
simpleClip = arr f where
	f x = if abs x <= 1.0 then x else signum x


s5 :: AudSF () Double
s5 = constA 1000 >>> vibrato 5 1000 >>> simpleClip

simpleInstr1 :: InstrumentName
simpleInstr1 = CustomInstrument "Simple Instrument"

myInstr :: Instr (AudSF () Double)
myInstr dur ap vol [vfrq,dep] =
    proc () -> do
        vib <- osc tab1 0 -< vfrq
        aud <- osc tab2 0 -< apToHz ap + vib * dep
        outA -< aud

type InstrMap1 a = [(InstrumentName, Instr a)]
-- makeitdb :: Music --D A F U Q
-- ???????
mel :: Music1
mel = 
    let m = Euterpea.line [na1 (c 4 en), na1 (ef 4 en), na1 (f 4 en),
         na2 (af 4 qn), na1 (f 4 en), na1 (af 4 en),
         na2 (bf 4 qn), na1 (af 4 en), na1 (bf 4 en),
         na1 (c 5 en), na1 (ef 5 en), na1 (f 5 en),
         na3 (af 5 wn)] where
             na1 (Prim (Note dr p)) = Prim (Note dr (p, [Params [0,0]]))
             na2 (Prim (Note dr p)) = Prim (Note dr (p, [Params [5,10]]))
             na3 (Prim (Note dr p)) = Prim (Note dr (p, [Params [5,20]]))
    in instrument simpleInstr1 m
prefixes :: [a] -> [[a]]
prefixes [] = []
prefixes (x:xs) = let f pf = x:pf
                  in [x]:map f (prefixes xs)
prefix :: [Music a] -> Music a
prefix mel = let m1 = line(concat(prefixes mel))
                 m2 = transpose 12 (line(concat(prefixes(reverse mel))))
                 m = instrument Flute m2 :=: instrument  m2 
              in m :+: transpose 5 m :+: m

rep :: (Music a -> Music a) -> (Music a -> Music a) -> Int -> Music a -> Music a
rep f g 0 m = rest 0
rep f g n m = m :=: g (rep f g (n-1) (f m))

myInstrMap :: InstrMap1 (AudSF () Double)
myInstrMap = [(simpleInstr1, myInstr)]

mel1 = prefix [es 3 qn, c 2 qn]

(dr,sf) = renderSF mel1 myInstrMap
main = outFile "simple.wav" dr sf

--why? 

    