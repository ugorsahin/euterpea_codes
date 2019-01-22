{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

import Euterpea

data Context a = Context {cTime :: PTime,
                          cPlayer :: Player a,
                          cInst :: InstrumentName,
                          cDur :: DurT,
                          cPch :: AbsPitch,
                          cVol :: Volume,
                          cKey :: (PitchClass,Mode)}

type PlayerName a = String a
type PMap a = PlayerName -> Player a

data Player a = MkPlayer {pName :: PlayerName,
                          playNote :: NoteFun a,
                          interpPhrase :: PhraseFun a,
                          notatePlayer :: NotateFun a}
type NoteFun a = MContext a -> Dur -> a -> Performance
type PhraseFun a = PMap a -> MContext a -> [PhraseAttribute] -> Music a -> (Performance,DurT)
type NotateFun a = ()
instance Show a => Show (Player a) where
    show p = "Player" ++ pName p

performa :: PMap a -> Context a -> Music a -> Performance
performa pm c m = fst (perf pm c m)

perf :: PMap a -> Context a -> Music a -> (Performance,DurT)
perf pm c@Context{cTime = t, cPlayer = pl, cDur = dt, cPch = k} m =
    case m of
        Prim (Note d p) -> (playNote pl c d p, d*dt)
        Prim (Rest d) -> ([],d*dt)
        m1 :+: m2 ->
            let (pf1,d1) = perf pm c m1
                (pf2,d2) = perf pm (c{cTime = t+d1}) m2
            in (pf1 ++ pf2, d1+d2)
        m1 :=: m2 ->
            let (pf1,d1) = perf pm c m1
                (pf2,d2) = perf pm c m2
            in (merge pf1 pf2, max d1 d2)
        Modify (Tempo r) m -> perf pm (c{cDur = dt/r}) m
        Modify (Transpose p) m -> perf pm (c{cPch = k + p}) m
        Modify (Instrument i) m -> perf pm (c{cInst = i}) m
        Modify (KeySig pc mo) m ->perf pm (c{cKey = (pc,mo)}) m
        Modify (Player pn) m -> perf pm (c{cPlayer = pm pn}) m
        Modify (Phrase pas) m -> interpPhrase pl pm c pas m



metro :: Int -> Dur -> DurT
metro settings dur = 60/(fromIntegral settings*dur)

defPlayer :: Player Note1
defPlayer = MkPlayer
            {pName = "Default",
             playNote = defPlayNote defNasHandler,
             interpPhrase = defInterpPhrase defPasHandler,
             notatePlayer = ()}

defPlayNote :: (Context (Pitch,[a]) -> a -> MEvent -> MEvent) -> NoteFun (Pitch,[a])
defPlayNote nasHandler c@(Context cTime cPlayer cInst cDur cPch cVol cKey) d (p,nas) = 
    let initEv = MEvent{eTime = cTime,
                       eDur = d * cDur,
                       eInst = cInst,
                       eVol = cVol,
                       ePitch = absPitch p + cPch,
                       eParams = []}
    in [foldr (nasHandler c) initEv nas]

defNasHandler :: Context a -> NoteAttribute -> MEvent -> MEvent
defNasHandler c (Volume v) ev = ev {eVol = v}
defNasHandler c (Params pms) ev = ev { eParams = pms}
defNasHandler _ _ ev = eVol

defInterpPhrase :: (PhraseAttribute -> Performance -> Performance) -> 
  (PMap a -> Context a ->[PhraseAttribute] -> Music a -> (Performance,DurT))
defInterpPhrase pasHandler pm context pas m =
    let (pf,dur) = perf pm context m
    in (foldr pasHandler pf pas,dur)
defPasHandler :: PhraseAttribute -> Performance -> Performance
defPasHandler (Dyn (Accent x)) = map (\e -> e{eVol = round(x*fromIntegral(eVol e))})
defPasHandler (Art (Staccato x)) = map (\e -> e {eDur = x * eDur e})
defPasHandler (Art (Legato x)) = map (\e -> e {eDur = x*eDur e})
defPasHandler _ = id

