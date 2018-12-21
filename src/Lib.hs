{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE StandaloneDeriving #-}

module Lib where

import Control.Applicative
import Euterpea.Music
import Euterpea.IO.MIDI.Play

deriving instance Foldable Music
deriving instance Foldable Primitive


maj7 :: PitchClass -> Octave -> [Pitch]
maj7 = curry $ \r ->
  [ r
  , trans 4 r
  , trans 7 r
  , trans 11 r
  ]

min7 :: PitchClass -> Octave -> [Pitch]
min7 = curry $ \r ->
  [ r
  , trans 3 r
  , trans 7 r
  , trans 10 r
  ]

dom7 :: PitchClass -> Octave -> [Pitch]
dom7 = curry $ \r ->
  [ r
  , trans 4 r
  , trans 7 r
  , trans 10 r
  ]


musicChord :: Dur -> [Pitch] -> Music Pitch
musicChord d = chord . fmap (note d)

progression :: [[Pitch]]
progression =
  [ min7 D 4
  , maj7 C 4
  , maj7 Bf 4
  , dom7 A 4
  ]

data Hand
    = LeftHand Int
    | RightHand Int
  deriving (Eq, Ord, Show)


myParadiddle17 :: Music Hand
myParadiddle17 = line
  [ note en $ LeftHand 0
  , note en $ RightHand 1
  , note en $ RightHand 2
  , note en $ LeftHand 3
  , note en $ RightHand 4
  , note en $ LeftHand 5
  , note en $ RightHand 6
  , note en $ RightHand 7
  ]


bind :: (Dur -> a -> Music b) -> Music a -> Music b
bind f (Prim (Note dur a)) = f dur a
bind f (Prim (Rest dur))   = Prim $ Rest dur
bind f (ma :+: mb)         = bind f ma :+: bind f mb
bind f (ma :=: mb)         = bind f ma :=: bind f mb
bind f (Modify control ma) = Modify control $ bind f ma


myBar :: [Pitch] -> Music Hand -> Music Pitch
myBar ((basspitch, bassoctave):remainder) paradiddle = bind f paradiddle
  where
    f dur (LeftHand _)  = note dur (basspitch, bassoctave - 1)
    f dur (RightHand _) = musicChord dur remainder


maybeAToPrim :: Dur -> Maybe a -> Music a
maybeAToPrim dur Nothing  = rest dur
maybeAToPrim dur (Just a) = note dur a


makeADiddleDoo :: Dur -> [Music Hand]
makeADiddleDoo totalDur
  | totalDur < 1/16 = pure $ rest 0
  | otherwise = do
      dur  <- [ totalDur, totalDur / 2 ]
      hand <- [ Just $ LeftHand 0
              , Just $ RightHand 0
              , Nothing
              ]
      following <- makeADiddleDoo $ totalDur - dur
      pure $ removeZeros $
        line
          [ maybeAToPrim dur hand
          , following
          ]


-- (Prim $ Note duration RightHand) ---->
--       ((Prim $ Note duration firstThingIntheTriad) :=: ())

-- playTheLeftHandBaby :: [Pitch] -> Pitch

