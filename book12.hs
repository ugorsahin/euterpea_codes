module Euterpea.Examples.RandomMusic where
import Euterpea
import System.Random
import System.Random.Distributions
import qualified Data.MarkovChain as M

sGen :: StdGen
sGen = mkStdGen 42 

randInts :: StdGen ->[Int]
randInts g = let (x,g') = next g
             in x:randInts g'

toAbsp1 :: Float -> AbsPitch
toAbsp1 x = round(40*x+30)

mkNote1 :: AbsPitch -> Music Pitch
mkNote1 = note sn . pitch

mkLine1 :: [AbsPitch] -> Music Pitch
mkLine1 rands = line(take 32 (map mkNote1 rands))

m1,m2 :: Music Pitch
m1 = mkLine1 (randomRs (30,70) sGen)
m2 = let rs1 = rands linear sGen
     in mkLine1 (map toAbsp1 rs1)

m3 :: Float -> Music Pitch
m3 lam = let rs1 = rands (exponential lam) sGen
         in mkLine1 (map toAbsp1 rs1)

m4 :: Float -> Float -> Music Pitch
m4 sig mu = let rs1 = rands (gaussian sig mu) sGen
            in mkLine1 (map toAbsp1 rs1)

m5 :: Float -> Music Pitch
m5 sig = let rs1 = rands (gaussian sig 0) sGen
         in mkLine2 50 (map toAbsp2 rs1)

m6 :: Float -> Music Pitch
m6 lam = let rs1 = rands (exponential lam) sGen
         in mkLine2 50 (map (toAbsp2 . subtract (1/lam)) rs1)

toAbsp2 :: Float -> AbsPitch
toAbsp2 x = round (5*x)

mkLine2 :: AbsPitch ->[AbsPitch] -> Music Pitch
mkLine2 start rands = line(take 64 (map mkNote1 (scanl (+) start rands)))

ps1,ps2 :: [Pitch]
ps1 = [(C,4),(D,4),(E,4),(F,4),(G,4),(A,4),(B,4)]
ps2 = [(C,4),(E,4),(G,4),(E,4),(F,4),(A,4),(G,4),(E,4),(C,4),(E,4),(G,4),(E,4),(F,4),(D,4),(C,4)]
duras = [qn,en,sn,en,sn,qn,en,wn,qn,en,sn,qn]

mc ps n = mkLine3 (M.run n ps 0 (mkStdGen 42))
mcm pss n = mkLine3 (concat (M.runMulti n pss 0 (mkStdGen 42)))

mkLine3 :: [Music Pitch] -> Music Pitch
mkLine3 ps = line (take 64 (ps))

