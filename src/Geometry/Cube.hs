module Geometry.Cube (cubeVolume, cubiodVolume) where

cubeVolume :: RealFloat a => a -> a
cubeVolume side = cubiodVolume side side side

cubiodVolume :: RealFloat a => a -> a -> a -> a
cubiodVolume l w h = l * w * h