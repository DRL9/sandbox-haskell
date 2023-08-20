module Geometry (sphereVolume) where

sphereVolume :: RealFloat a => a -> a
sphereVolume r = pi * r ^ 3
