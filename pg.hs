import Euterpea
import Book17
import qualified Data.ByteString.Lazy as BL

opening :: Music Pitch
opening = tempo (96/60) $ takeM 5 $ repeatM $ (note (1/3) (F,5) :+: note (1/3) (Af,5))

opening2 :: Music Pitch
opening2 = tempo (120/60) $ takeM 5 $ repeatM $ (note (1/2) (Af,4) :+: note (1/2) (C,5))

philipMode :: (Pitch,Pitch) -> Dur -> Dur -> Music Pitch
philipMode (f,s) dur ctr = tempo (96/60) $ takeM ctr $ repeatM $ (note dur f :+: note dur s)

openingStart  = note 4 (F,3) :=: 
                            philipMode ((Af,3),(C,4)) (1/2) 8 :=: 
                            (philipMode ((F,4),(Af,4)) (1/3) 4 :+: philipMode ((C,5),(F,4)) (1/3) 4)
openingStart2 = note 4 (Ef,3) :=: philipMode ((G,3),(Bf,3)) (1/2) 8 
                              :=: (philipMode ((G,4),(C,5)) (1/3) 4 :+: (philipMode ((Gs,4),(C,5)) (1/3) (10/3) :+: philipMode ((G,4),(C,5)) (1/3) (2/3)))


			
rob :: IO ()
rob = playDev 2 $ tempo 2 (openingStart :+: openingStart2 :+: openingStart :+: openingStart2)

main :: IO ()
main = do
    contents <- BL.getContents
    BL.putStr contents