-- Gary is an avid hiker. He tracks his hikes meticulously, paying
-- close attention to small details like topography. During his last
-- hike he took exactly n steps. For every step he took, he noted if it
-- was an uphill, U , or a downhill, D step. Gary's hikes start and end at
-- sea level and each step up or down represents a unit 1 change in
-- altitude. We define the following terms:

-- A mountain is a sequence of consecutive steps above sea level,
-- starting with a step up from sea level and ending with a step down
-- to sea level.

-- A valley is a sequence of consecutive steps below sea level,
-- starting with a step down from sea level and ending with a step up
-- to sea level.

-- Given Gary's sequence of up and down steps during his last hike,
-- find and print the number of valleys he walked through.

-- For example, if Gary's path is s = "DDUUUUDD", he
-- first enters a valley 2 units deep. Then he climbs out an up onto a
-- mountain 2 units high. Finally, he returns tof sea level and ends his
-- hike.

-- Function Description

-- Complete the countingValleys function in the editor below. It must
-- return an integer that denotes the number of valleys Gary
-- traversed.

-- countingValleys has the following parameter(s):

-- n: the number of steps Gary takes
-- s: a string describing his path

module CountingValleys where

import Data.List as List

countingValleys :: Int -> String -> Int
countingValleys _  = sum . zipSelfWith markOut . List.scanl' step 0 where
  step s 'D' = s - 1
  step s 'U' = s + 1
  step s _   = s
  zipSelfWith f xs = zipWith f xs (tail xs)
  markOut (-1) 0 = 1
  markOut _ _  = 0
