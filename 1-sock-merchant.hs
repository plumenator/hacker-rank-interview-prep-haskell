-- John works at a clothing store. He has a large pile of socks that
-- he must pair by color for sale. Given an array of integers
-- representing the color of each sock, determine how many pairs of
-- socks with matching colors there are.

-- For example, there are socks with colors . There is one pair of
-- color and one of color . There are three odd socks left, one of
-- each color. The number of pairs is .

-- Function Description

-- Complete the sockMerchant function in the editor below. It must
-- return an integer representing the number of matching pairs of
-- socks that are available.

-- sockMerchant has the following parameter(s):

-- n: the number of socks in the pile ar: the colors of each sock

module SockMerchant where
import Data.Map.Strict as Map
import Data.List as List

sockMerchant :: Int -> [Int] -> Int
sockMerchant _ = Map.foldl' (+) 0 . Map.map (`div` 2) . List.foldl' insert Map.empty where
  insert m k = Map.insertWith (+) k 1 m
