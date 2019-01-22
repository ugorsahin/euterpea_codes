module Book17 
  where

import Euterpea
import Data.Maybe (mapMaybe)


lel :: String -> Bool 
lel _ = True  


t251 :: Music Pitch
t251 = let dMinor = d 4 wn :+: g 4 wn
        in dMinor

twoFiveOne:: Pitch -> Dur -> Music Pitch
twoFiveOne p d = let minorTwo = (note d p :=: note d (trans 4 p) :=: note d (trans 7 p))
                                :+: (note d (trans 7 p) :=: note d (trans 10 p) :=: note d (trans 14 p) )
                                :+: (note d (trans 2 p) :=: note d (trans 6 p) :=: note d (trans 9 p) )
                 in minorTwo 


dene :: Pitch -> [Int] -> Dur -> Music Pitch
dene x [] d = note d x :+: rest 0
dene x (a:as) d = note d x :+: dene (trans a x) as d

transM:: AbsPitch -> Music Pitch -> Music Pitch
transM ap (Prim (Note d p)) = note d (trans ap p)
transM ap (Prim (Rest d)) = rest d
transM ap (m1 :+: m2) = transM ap m1 :+: transM ap m2
transM ap (m1 :=: m2) = transM ap m1 :=: transM ap m2

aynianda :: Pitch -> [Int] -> Dur -> Music Pitch
aynianda x [] d = note d x :+: rest 0
aynianda x (a:as) d = note d x :=: aynianda (trans a x) as d

grow :: Double -> Double -> Double
grow r x = r * x * (1-x)

popToNote :: Double -> [MidiMessage]
popToNote x = [ANote 0 n 64 0.05] where n = truncate (x * 127)

wts :: Pitch -> [Music Pitch]
wts p = let f ap = note wn (pitch (absPitch p + ap))
        in map f[1,3,4]

wtf :: [Music Pitch] -> Music Pitch
wtf [] = rest 0
wtf (x:xs) = x :=: wtf xs

hop :: Pitch -> Music Pitch
hop a = wtf (wts a)

timesn :: Int -> Music a -> Music a
timesn 0 m = rest 0
timesn n m = m :+: timesn (n-1) m

addDur :: Dur -> [Dur -> Music a] -> Music a
addDur d ns = line [n d | n <- ns]

graceNote :: Int ->Music Pitch -> Music Pitch
graceNote n (Prim (Note d p)) = note (d/8) (trans n p) :+: note(7*(d/8)) p
graceNote n _ = error "NOO"

childSong2 :: Music Pitch
childSong2 = let t = (dhn/qn) * (69/120)
             in instrument RhodesPiano
                           (tempo t (bassLine :=: mainVoice))

prefixes :: [a] -> [[a]]
prefixes [] = []
prefixes (x:xs) = let f pf = x:pf
                  in [x]:map f (prefixes xs)

prefix :: [Music a] -> Music a
prefix mel = let m1 = line(concat(prefixes mel))
                 m2 = transpose 12 (line(concat(prefixes(reverse mel))))
                 m = instrument Flute m2 :=: instrument VoiceOohs m2 
              in m :+: transpose 5 m :+: m

playab :: [AbsPitch] ->[AbsPitch]
playab = filter (<100)

apPairs :: [AbsPitch] -> [AbsPitch] -> [(AbsPitch,AbsPitch)]
apPairs [] _ = []
apPairs _ [] = []
apPairs (x:xs) (y:ys)
    | (abs (x-y) <= 8 && abs(x-y) >= 2) = [(x,y)] ++ apPairs xs ys
    | otherwise = apPairs xs ys

toMu :: [(AbsPitch,AbsPitch)] -> Music Pitch
toMu [] = rest 0;
toMu ((x,y):xs)
    | (toInteger(x + y)) > 100 = note (1/2) (pitch x) :=: note qn (pitch y) :+: toMu xs
    | otherwise = note (1) (pitch x) :=: note qn (pitch y) :+: toMu xs
b1 = addDur qn [b 3, fs 4,g 4, fs 4]
b2 = addDur qn [b 3, es 4, fs 4, es 4]
b3 = addDur qn [as 3, fs 4, g 4, fs 4]

lineToL :: Music a -> [Music a]
lineToL (Prim (Rest _)) = []
lineToL (n :+: ns) = n:lineToL ns
lineToL _ = error "PLS"

(=:=) :: Dur -> Dur -> Music a -> Music a
old =:= new = tempo (new/old)

invort :: Music Pitch -> Music Pitch
invort  m = line (map inv l)
            where 
            l@(Prim (Note _ r):_) = lineToL m
            inv (Prim (Note d p)) = note d (pitch(2*absPitch r - absPitch p))
            inv (Prim (Rest d)) = rest d

retro1,retroIn,inRetro :: Music Pitch -> Music Pitch
retro1 = line . reverse . lineToL
retroIn = retro1 . invort
inRetro = invort . retro1

hepp :: Music Pitch -> Music Pitch
hepp a = a :+: retro1 a :+: retroIn a :+: inRetro a

dura :: Music a -> Dur
dura (Prim (Note d _)) = d
dura (Prim (Rest d)) = d
dura (m1 :+: m2) = dura m1 + dura m2
dura (m1 :=: m2) = dura m1 `max` dura m2
dura (Modify (Tempo r) m) = dura m/r
dura (Modify _ m) = dura m

repeatM :: Music a -> Music a
repeatM m = m :+: repeatM m

delayM :: Dur -> Music a -> Music a
delayM d m = rest d :+: m

pr1, pr2 :: Pitch -> Music Pitch
pr1 p = tempo (5/6) (tempo (4/3) (mkLn 1 p qn :+:
                                 tempo (3/2) (mkLn 3 p en:+:
                                               mkLn 2 p sn:+:
                                               mkLn 1 p qn) :+:
                                 mkLn 1 p qn) :+:
                            tempo(3/2) (mkLn 6 p en))
pr2 p = tempo (7/6) (ma :+: tempo (5/4) (mkLn 5 p en) :+: ma :+: tempo (3/2) mb)
        where
            ma = tempo (5/4) (tempo (3/2) mb :+: mb)
            mb = mkLn 3 p en

mkLn :: Int -> p -> Dur -> Music p
mkLn n p d = line $ take n $ repeat $ note d p

revM :: Music a -> Music a
revM n@(Prim _) = n
revM (Modify c m) = Modify c (revM m)
revM (m1 :+: m2) = revM m2 :+: revM m1
revM (m1 :=: m2) = if d1>d2 then revM m1 :=: (rest (d1-d2) :+: revM m2) else (rest (d2-d1) :+: revM m1) :=: revM m2
                   where
                    d1 = dur m1
                    d2 = dur m2

takeM :: Dur -> Music a -> Music a
takeM d m | d <= 0 = rest 0
takeM d (Prim (Note oldD p)) = note (min oldD d) p
takeM d (Prim (Rest oldD)) = rest (min oldD d)
takeM d (m1 :=: m2) = takeM d m1 :=: takeM d m2
takeM d (m1 :+: m2) = let mm1 = takeM d m1
                          mm2 = takeM (d - dur mm1) m2
                          in mm1 :+: mm2
takeM d (Modify (Tempo r) m) = tempo r (takeM (d*r) m)
takeM d (Modify c m) = Modify c (takeM d m)

trill :: Int -> Dur -> Music Pitch -> Music Pitch
trill i sDur (Prim (Note tDur p))
    | sDur >= tDur = note tDur p
    | otherwise = note sDur p :+: trill (negate i) sDur (note (tDur - sDur) (trans i p))
trill i d (Modify (Tempo r) m) = tempo r (trill i (d*r) m)
trill i d (Modify c m) = Modify c (trill i d m)
trill _ _ _ = error "dude cmn"

trilln :: Int -> Int -> Music Pitch -> Music Pitch
trilln i nTimes m = trill i (dur m/fromIntegral nTimes) m

roll :: Dur -> Music Pitch -> Music Pitch
rolln :: Int -> Music Pitch -> Music Pitch
roll dur m = trill 0 dur m
rolln nTimes m = trilln 0 nTimes m

ssfMel :: Music Pitch
ssfMel = line (l1 ++ l2 ++ l3 ++ l4)
         where 
             l1 = [trilln 2 5 (bf 6 en), ef 7 en, ef 6 en, ef 7 en]
             l2 = [bf 6 sn, c 7 sn, bf 6 sn, g 6 sn, ef 6 en, bf 5 en]
             l3 = [ef 6 sn, f 6 sn, g 6 sn, af 6 sn, bf 6 en, ef 7 en]
             l4 = [trill 2 tn (bf 6 qn), bf 6 sn, denr]
sands :: Music Pitch
sands = instrument Flute ssfMel

majchord :: Dur -> Pitch -> Music Pitch
majchord d p = note d p :=: note d (trans 4 p) :=: note d (trans 7 p)

grace :: Int -> Rational -> Music Pitch -> Music Pitch
grace n r (Prim (Note d p)) = note (r * d) (trans n p) :+: note((1-r)*d) p
grace n r _ = error "grace: can only add a grace note"

mordent :: Int -> Music Pitch -> Music Pitch
mordent n k@(Prim (Note d p)) = k :+: note d (trans n p) :+: k

appog :: Int -> Music Pitch -> Music Pitch
appog a k@(Prim (Note d p )) = (k :+: note (d/8) (trans a p) :+: k) :=:c  
                               where
                                c = majchord ((5*d)/2) (trans (-12) p)

funkGroove :: Music Pitch
funkGroove = tempo 1 $ instrument Percussion $ takeM 8 $ repeatM ((p1 :+: qnr :+: p2 :+: qnr :+: p1 :+: p1 :+: qnr :+:p2 :+: enr) :=: roll en (perc ClosedHiHat 2))
             where p1 = perc LowTom qn
                   p2 = perc AcousticSnare en 


strangething :: [Int] -> Music Pitch
strangething [] = rest 0
strangething (x:xs) = perc (toEnum x::PercussionSound) qn :+: strangething xs

rep :: (Music a -> Music a) -> (Music a -> Music a) -> Int -> Music a -> Music a
rep f g 0 m = rest 0
rep f g n m = m :=: g (rep f g (n-1) (f m))



mel1 = prefix [es 3 qn, c 2 qn]

la = addDur qn [b 2, fs 3, g 3]
la1 = addDur qn [b 2,fs 3]

bassLine = timesn 3 b1 :+: timesn 2 b2 :+: timesn 4 b3 :+: timesn 5 b1
mainVoice = timesn 4 v1 :+: v2


v1 = v1a :+: graceNote (-1) (d 5 qn) :+: v1b
v1a = addDur en [a 5, e 5,d 5,fs 5, cs 5, b 4, e 5, b 4]
v1b = addDur en [cs 5, b 4]

v2 = v2a :+: v2b :+: v2c :+: v2d :+: v2e :+: v2f :+: v2g

v2a=line[cs 5 (dhn+dhn),d 5 dhn,f 5 hn,gs 5 qn,fs 5 (hn+en),g 5 en]  -- bars 7-11
v2b=addDur en [fs 5,e 5,cs 5,as 4] :+: a 4 dqn :+: addDur en [as 4,cs 5,fs 5,e 5,fs 5]-- bars 12-13
v2c=line [g 5 en, as 5 en, cs 6 (hn+en),d 6 en,cs 6 en] :+: e 5 en:+:enr:+:line[as 5 en, a 5 en, g 5 en, d 5 qn,c 5 en,cs 5 en]-- bars 14-16
v2d=addDur en [fs 5,cs 5,e 5,cs 5,a 4,as 4,d 5,e 5,fs 5]-- bars 17-18.5
v2e=line [graceNote 2 (e 5 qn),d 5 en, graceNote 2 (d 5 qn),cs 5 en, graceNote 1 (cs 5 qn), b 4 (en+hn), cs 5 en, b 4 en]-- bars 18.5-20
v2f=line [fs 5 en,a 5 en,b 5 (hn+qn),a 5 en,fs 5 en,e 5 qn,d 5 en,fs 5 en,e 5 hn,d 5 hn,fs 5 qn]  -- bars 21-23
v2g=tempo (3/2) (line [cs 5 en,d 5 en,cs 5 en]) :+:b 4 (3 * dhn+hn)-- bars 24-28



run = rep (transpose 5) (delayM tn) 8 (c 4 tn)
cascade = rep (transpose 4) (delayM en) 8 run
cascades = rep id (delayM sn) 2 cascade

final = cascades :+: revM cascades
