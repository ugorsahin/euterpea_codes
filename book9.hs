module Euterpea.Examples.SelfSimilar where
import Euterpea

data Cluster = Cluster SNote [Cluster]
    deriving (Show)
type SNote = (Dur, AbsPitch)

revM :: Music a -> Music a
revM n@(Prim _) = n
revM (Modify c m) = Modify c (revM m)
revM (m1 :+: m2) = revM m2 :+: revM m1
revM (m1 :=: m2) = if d1>d2 then revM m1 :=: (rest (d1-d2) :+: revM m2) else (rest (d2-d1) :+: revM m1) :=: revM m2
                   where
                    d1 = dur m1
                    d2 = dur m2

selfSim :: [SNote] -> Cluster
selfSim pat = Cluster (0,0) (map mkCluster pat) 
    where mkCluster note = Cluster note (map (mkCluster . addMult note) pat)

addMult :: SNote -> SNote -> SNote
addMult (d0,p0) (d1,p1) = (d0*d1,p0+p1)

fringe :: Int -> Cluster -> [SNote]
fringe 0 (Cluster note cls) = [note]
fringe n (Cluster note cls) = concatMap (fringe (n-1)) cls

simToMusic :: [SNote] ->Music Pitch
simToMusic = line . map mkNote

mkNote :: (Dur,AbsPitch) -> Music Pitch
mkNote (d,ap) = note d (pitch ap)

fringe' :: Int -> Cluster -> [[SNote]]
fringe' 0 (Cluster note cls) = [[note]]
fringe' n (Cluster note cls) = map (fringe (n-1)) cls

simToMusic' :: [[SNote]] -> Music Pitch
simToMusic' = chord . map (line . map mkNote)

ss' pat n tr te = transpose tr $ tempo te $ simToMusic' $fringe' n $ selfSim pat

ss pat n tr te = transpose tr $ tempo te $ simToMusic $fringe n $ selfSim pat

m0,m1 :: [SNote]
m0 = [(1,2),(1,0),(1,5),(1,7)]
tm0 = instrument Vibraphone (ss m0 3 50 20)
ttm0 = tm0 :=: transpose (-12) tm0
m1 = [(1,0),(0.5,0),(0.5,0)]
tm1 = instrument Percussion (ss m1 2 43 2)
m2 :: [SNote]
m2 = [(dqn,0),(qn,4)]
tm2 = ss m2 4 50 (1/50)
m3 :: [SNote]
m3 = [(hn,3),(qn,4),(qn,0),(hn,6)]
tm3 = ss m3 4 50 (1/4)
ttm3 = let l1 = instrument Flute tm3
           l2 = instrument AcousticBass $ transpose (-9) (revM tm3)
           in l1 :=: l2
m4 :: [SNote]
m4 = [(hn,3),(hn,8),(hn,22),(qn,4),(qn,7),(qn,21),(qn,0),(qn,5),(qn,15),(wn,6),(wn,9),(wn,19)]
tm4 = ss m4 3 50 8

ss1 = ss' m2 4 50 (1/8)
ss2 = ss' m3 4 50 (1/2)
ss3 = ss' m4 3 50 2

m5 = [(en,7),(sn,4),(en,7),(en,9),(sn,11),(en,14),(en,16),(sn,19),(en,21)]
ss5 = ss m5 4 30 (1/500)
ss6 = ss' m5 4 30 (1/1000)