module Lib where

import Euterpea.Music
import Euterpea.IO.MIDI.Play


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
    = LeftHand
    | RightHand
  deriving (Eq, Ord, Show)


myParadiddle17 :: Music Hand
myParadiddle17 = line
  [ note en LeftHand
  , note en RightHand
  , note en RightHand
  , note en LeftHand
  , note en RightHand
  , note en LeftHand
  , note en RightHand
  , note en RightHand
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
    f dur LeftHand  = note dur (basspitch, bassoctave - 2)
    f dur RightHand = musicChord dur remainder


-- (Prim $ Note duration RightHand) ---->
--       ((Prim $ Note duration firstThingIntheTriad) :=: ())

-- playTheLeftHandBaby :: [Pitch] -> Pitch

