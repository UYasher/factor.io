module ResourcePrinting where

import Blueprint
import BlueprintParsing
import Factory
import Geometry
import Machine
import ResourceUpdate
import State

-- This file is for internal debugging use only. It (arcanely) converts `Resources` into a string

resourcesToString :: Int -> Int -> Resources -> String
resourcesToString width height r = unlines $ map (\h -> rtsLine width h r) [height - 1, height - 2 .. 0]

rtsLine :: Int -> Int -> Resources -> String
rtsLine width h r = map (\w -> rtsChar w h r) [0 .. width - 1]

rtsChar :: Int -> Int -> Resources -> Char
rtsChar w h r =
  let p = Point w h
      mx = evalState (getHoriz p) r
      my = evalState (getVert p) r
   in case (mx, my) of
        (Just x, Nothing) -> valueToChar x
        (Nothing, Just y) -> valueToChar y
        (Just _, Just _) -> '.'
        (Nothing, Nothing) -> ' '

stepNToStrings :: Int -> Blueprint -> [String]
stepNToStrings n b = blueprintToString b : aux 0 n (makeFactory b) emptyResources
  where
    aux :: Int -> Int -> Maybe Factory -> Resources -> [String]
    aux _ _ Nothing _ = []
    aux i end _ _ | i >= end = []
    aux i end (Just f) r =
      let head = resourcesToString (width b) (height b) r
          rest = aux (i + 1) end (Just f) (execState f r)
       in ("\n" ++ show (i + 1) ++ " of " ++ show n ++ "\n") : head : rest