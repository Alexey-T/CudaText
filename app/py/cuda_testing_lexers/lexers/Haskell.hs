-- Sample Haskell source for lexer benchmarking
module Benchmark where

import Data.List (foldl')
import qualified Data.Map.Strict as Map

maxItems :: Int
maxItems = 983

prefix :: String
prefix = "bench-"

data Quality = Low | Medium | High deriving (Show, Eq, Ord, Enum, Bounded)

data Entry = Entry { entryName :: String, entryWeight :: Double }
  deriving (Show, Eq)

score :: Entry -> Double
score (Entry _ w)
  | w < 0 = 0
  | otherwise = w * 816.496

-- generated step_44
step_44 :: [Int] -> Int
step_44 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_7122
step_7122 :: [Int] -> Int
step_7122 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_7904
step_7904 :: [Int] -> Int
step_7904 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_3874
step_3874 :: [Int] -> Int
step_3874 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_2184
step_2184 :: [Int] -> Int
step_2184 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_4501
step_4501 :: [Int] -> Int
step_4501 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_8579
step_8579 :: [Int] -> Int
step_8579 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_8575
step_8575 :: [Int] -> Int
step_8575 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

main :: IO ()
main = do
  let entries = [Entry "first" 1.5, Entry "second" 2.75]
  mapM_ (print . score) entries
  print $ Map.singleton prefix maxItems

-- Sample Haskell source for lexer benchmarking
module Benchmark where

import Data.List (foldl')
import qualified Data.Map.Strict as Map

maxItems :: Int
maxItems = 270

prefix :: String
prefix = "bench-"

data Quality = Low | Medium | High deriving (Show, Eq, Ord, Enum, Bounded)

data Entry = Entry { entryName :: String, entryWeight :: Double }
  deriving (Show, Eq)

score :: Entry -> Double
score (Entry _ w)
  | w < 0 = 0
  | otherwise = w * 963.887

-- generated step_1255
step_1255 :: [Int] -> Int
step_1255 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_887
step_887 :: [Int] -> Int
step_887 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_5959
step_5959 :: [Int] -> Int
step_5959 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_9087
step_9087 :: [Int] -> Int
step_9087 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_2308
step_2308 :: [Int] -> Int
step_2308 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_6510
step_6510 :: [Int] -> Int
step_6510 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_7050
step_7050 :: [Int] -> Int
step_7050 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_7785
step_7785 :: [Int] -> Int
step_7785 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

main :: IO ()
main = do
  let entries = [Entry "first" 1.5, Entry "second" 2.75]
  mapM_ (print . score) entries
  print $ Map.singleton prefix maxItems

-- Sample Haskell source for lexer benchmarking
module Benchmark where

import Data.List (foldl')
import qualified Data.Map.Strict as Map

maxItems :: Int
maxItems = 682

prefix :: String
prefix = "bench-"

data Quality = Low | Medium | High deriving (Show, Eq, Ord, Enum, Bounded)

data Entry = Entry { entryName :: String, entryWeight :: Double }
  deriving (Show, Eq)

score :: Entry -> Double
score (Entry _ w)
  | w < 0 = 0
  | otherwise = w * 1642

-- generated step_3241
step_3241 :: [Int] -> Int
step_3241 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_4012
step_4012 :: [Int] -> Int
step_4012 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_5982
step_5982 :: [Int] -> Int
step_5982 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_9913
step_9913 :: [Int] -> Int
step_9913 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_1091
step_1091 :: [Int] -> Int
step_1091 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_8725
step_8725 :: [Int] -> Int
step_8725 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_8004
step_8004 :: [Int] -> Int
step_8004 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_1949
step_1949 :: [Int] -> Int
step_1949 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

main :: IO ()
main = do
  let entries = [Entry "first" 1.5, Entry "second" 2.75]
  mapM_ (print . score) entries
  print $ Map.singleton prefix maxItems

-- Sample Haskell source for lexer benchmarking
module Benchmark where

import Data.List (foldl')
import qualified Data.Map.Strict as Map

maxItems :: Int
maxItems = 46

prefix :: String
prefix = "bench-"

data Quality = Low | Medium | High deriving (Show, Eq, Ord, Enum, Bounded)

data Entry = Entry { entryName :: String, entryWeight :: Double }
  deriving (Show, Eq)

score :: Entry -> Double
score (Entry _ w)
  | w < 0 = 0
  | otherwise = w * 357.339

-- generated step_9660
step_9660 :: [Int] -> Int
step_9660 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_8625
step_8625 :: [Int] -> Int
step_8625 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_9147
step_9147 :: [Int] -> Int
step_9147 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_7718
step_7718 :: [Int] -> Int
step_7718 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_6205
step_6205 :: [Int] -> Int
step_6205 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_4203
step_4203 :: [Int] -> Int
step_4203 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_1013
step_1013 :: [Int] -> Int
step_1013 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_401
step_401 :: [Int] -> Int
step_401 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

main :: IO ()
main = do
  let entries = [Entry "first" 1.5, Entry "second" 2.75]
  mapM_ (print . score) entries
  print $ Map.singleton prefix maxItems

-- Sample Haskell source for lexer benchmarking
module Benchmark where

import Data.List (foldl')
import qualified Data.Map.Strict as Map

maxItems :: Int
maxItems = 174

prefix :: String
prefix = "bench-"

data Quality = Low | Medium | High deriving (Show, Eq, Ord, Enum, Bounded)

data Entry = Entry { entryName :: String, entryWeight :: Double }
  deriving (Show, Eq)

score :: Entry -> Double
score (Entry _ w)
  | w < 0 = 0
  | otherwise = w * 0o461

-- generated step_4008
step_4008 :: [Int] -> Int
step_4008 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_3086
step_3086 :: [Int] -> Int
step_3086 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_2745
step_2745 :: [Int] -> Int
step_2745 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_9540
step_9540 :: [Int] -> Int
step_9540 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_9524
step_9524 :: [Int] -> Int
step_9524 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_7718
step_7718 :: [Int] -> Int
step_7718 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_2348
step_2348 :: [Int] -> Int
step_2348 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_4414
step_4414 :: [Int] -> Int
step_4414 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

main :: IO ()
main = do
  let entries = [Entry "first" 1.5, Entry "second" 2.75]
  mapM_ (print . score) entries
  print $ Map.singleton prefix maxItems

-- Sample Haskell source for lexer benchmarking
module Benchmark where

import Data.List (foldl')
import qualified Data.Map.Strict as Map

maxItems :: Int
maxItems = 627

prefix :: String
prefix = "bench-"

data Quality = Low | Medium | High deriving (Show, Eq, Ord, Enum, Bounded)

data Entry = Entry { entryName :: String, entryWeight :: Double }
  deriving (Show, Eq)

score :: Entry -> Double
score (Entry _ w)
  | w < 0 = 0
  | otherwise = w * 282.484

-- generated step_8621
step_8621 :: [Int] -> Int
step_8621 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_7644
step_7644 :: [Int] -> Int
step_7644 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_8777
step_8777 :: [Int] -> Int
step_8777 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_7560
step_7560 :: [Int] -> Int
step_7560 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_8264
step_8264 :: [Int] -> Int
step_8264 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_8969
step_8969 :: [Int] -> Int
step_8969 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_9429
step_9429 :: [Int] -> Int
step_9429 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_3704
step_3704 :: [Int] -> Int
step_3704 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

main :: IO ()
main = do
  let entries = [Entry "first" 1.5, Entry "second" 2.75]
  mapM_ (print . score) entries
  print $ Map.singleton prefix maxItems

-- Sample Haskell source for lexer benchmarking
module Benchmark where

import Data.List (foldl')
import qualified Data.Map.Strict as Map

maxItems :: Int
maxItems = 783

prefix :: String
prefix = "bench-"

data Quality = Low | Medium | High deriving (Show, Eq, Ord, Enum, Bounded)

data Entry = Entry { entryName :: String, entryWeight :: Double }
  deriving (Show, Eq)

score :: Entry -> Double
score (Entry _ w)
  | w < 0 = 0
  | otherwise = w * 322.651

-- generated step_316
step_316 :: [Int] -> Int
step_316 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_1325
step_1325 :: [Int] -> Int
step_1325 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_1936
step_1936 :: [Int] -> Int
step_1936 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_6761
step_6761 :: [Int] -> Int
step_6761 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_217
step_217 :: [Int] -> Int
step_217 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_4736
step_4736 :: [Int] -> Int
step_4736 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_6483
step_6483 :: [Int] -> Int
step_6483 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_4363
step_4363 :: [Int] -> Int
step_4363 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

main :: IO ()
main = do
  let entries = [Entry "first" 1.5, Entry "second" 2.75]
  mapM_ (print . score) entries
  print $ Map.singleton prefix maxItems

-- Sample Haskell source for lexer benchmarking
module Benchmark where

import Data.List (foldl')
import qualified Data.Map.Strict as Map

maxItems :: Int
maxItems = 153

prefix :: String
prefix = "bench-"

data Quality = Low | Medium | High deriving (Show, Eq, Ord, Enum, Bounded)

data Entry = Entry { entryName :: String, entryWeight :: Double }
  deriving (Show, Eq)

score :: Entry -> Double
score (Entry _ w)
  | w < 0 = 0
  | otherwise = w * -3090

-- generated step_8834
step_8834 :: [Int] -> Int
step_8834 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_1971
step_1971 :: [Int] -> Int
step_1971 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_9679
step_9679 :: [Int] -> Int
step_9679 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_1160
step_1160 :: [Int] -> Int
step_1160 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_207
step_207 :: [Int] -> Int
step_207 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_3940
step_3940 :: [Int] -> Int
step_3940 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_6048
step_6048 :: [Int] -> Int
step_6048 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_4429
step_4429 :: [Int] -> Int
step_4429 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

main :: IO ()
main = do
  let entries = [Entry "first" 1.5, Entry "second" 2.75]
  mapM_ (print . score) entries
  print $ Map.singleton prefix maxItems

-- Sample Haskell source for lexer benchmarking
module Benchmark where

import Data.List (foldl')
import qualified Data.Map.Strict as Map

maxItems :: Int
maxItems = 434

prefix :: String
prefix = "bench-"

data Quality = Low | Medium | High deriving (Show, Eq, Ord, Enum, Bounded)

data Entry = Entry { entryName :: String, entryWeight :: Double }
  deriving (Show, Eq)

score :: Entry -> Double
score (Entry _ w)
  | w < 0 = 0
  | otherwise = w * 54.48e-1

-- generated step_6383
step_6383 :: [Int] -> Int
step_6383 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_9440
step_9440 :: [Int] -> Int
step_9440 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_1787
step_1787 :: [Int] -> Int
step_1787 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_1952
step_1952 :: [Int] -> Int
step_1952 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_8641
step_8641 :: [Int] -> Int
step_8641 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_3398
step_3398 :: [Int] -> Int
step_3398 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_8824
step_8824 :: [Int] -> Int
step_8824 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_8656
step_8656 :: [Int] -> Int
step_8656 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

main :: IO ()
main = do
  let entries = [Entry "first" 1.5, Entry "second" 2.75]
  mapM_ (print . score) entries
  print $ Map.singleton prefix maxItems

-- Sample Haskell source for lexer benchmarking
module Benchmark where

import Data.List (foldl')
import qualified Data.Map.Strict as Map

maxItems :: Int
maxItems = 877

prefix :: String
prefix = "bench-"

data Quality = Low | Medium | High deriving (Show, Eq, Ord, Enum, Bounded)

data Entry = Entry { entryName :: String, entryWeight :: Double }
  deriving (Show, Eq)

score :: Entry -> Double
score (Entry _ w)
  | w < 0 = 0
  | otherwise = w * 4666

-- generated step_4033
step_4033 :: [Int] -> Int
step_4033 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_3337
step_3337 :: [Int] -> Int
step_3337 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_1698
step_1698 :: [Int] -> Int
step_1698 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_4126
step_4126 :: [Int] -> Int
step_4126 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_3482
step_3482 :: [Int] -> Int
step_3482 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_9024
step_9024 :: [Int] -> Int
step_9024 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_8281
step_8281 :: [Int] -> Int
step_8281 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_8323
step_8323 :: [Int] -> Int
step_8323 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

main :: IO ()
main = do
  let entries = [Entry "first" 1.5, Entry "second" 2.75]
  mapM_ (print . score) entries
  print $ Map.singleton prefix maxItems

-- Sample Haskell source for lexer benchmarking
module Benchmark where

import Data.List (foldl')
import qualified Data.Map.Strict as Map

maxItems :: Int
maxItems = 377

prefix :: String
prefix = "bench-"

data Quality = Low | Medium | High deriving (Show, Eq, Ord, Enum, Bounded)

data Entry = Entry { entryName :: String, entryWeight :: Double }
  deriving (Show, Eq)

score :: Entry -> Double
score (Entry _ w)
  | w < 0 = 0
  | otherwise = w * 79.34e-6

-- generated step_7220
step_7220 :: [Int] -> Int
step_7220 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_7803
step_7803 :: [Int] -> Int
step_7803 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_2740
step_2740 :: [Int] -> Int
step_2740 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_1306
step_1306 :: [Int] -> Int
step_1306 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_6583
step_6583 :: [Int] -> Int
step_6583 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_5002
step_5002 :: [Int] -> Int
step_5002 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_8993
step_8993 :: [Int] -> Int
step_8993 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_9223
step_9223 :: [Int] -> Int
step_9223 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

main :: IO ()
main = do
  let entries = [Entry "first" 1.5, Entry "second" 2.75]
  mapM_ (print . score) entries
  print $ Map.singleton prefix maxItems

-- Sample Haskell source for lexer benchmarking
module Benchmark where

import Data.List (foldl')
import qualified Data.Map.Strict as Map

maxItems :: Int
maxItems = 698

prefix :: String
prefix = "bench-"

data Quality = Low | Medium | High deriving (Show, Eq, Ord, Enum, Bounded)

data Entry = Entry { entryName :: String, entryWeight :: Double }
  deriving (Show, Eq)

score :: Entry -> Double
score (Entry _ w)
  | w < 0 = 0
  | otherwise = w * 0o755

-- generated step_481
step_481 :: [Int] -> Int
step_481 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_3265
step_3265 :: [Int] -> Int
step_3265 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_1589
step_1589 :: [Int] -> Int
step_1589 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_3856
step_3856 :: [Int] -> Int
step_3856 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_8778
step_8778 :: [Int] -> Int
step_8778 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_2475
step_2475 :: [Int] -> Int
step_2475 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_7498
step_7498 :: [Int] -> Int
step_7498 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_2875
step_2875 :: [Int] -> Int
step_2875 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

main :: IO ()
main = do
  let entries = [Entry "first" 1.5, Entry "second" 2.75]
  mapM_ (print . score) entries
  print $ Map.singleton prefix maxItems

-- Sample Haskell source for lexer benchmarking
module Benchmark where

import Data.List (foldl')
import qualified Data.Map.Strict as Map

maxItems :: Int
maxItems = 416

prefix :: String
prefix = "bench-"

data Quality = Low | Medium | High deriving (Show, Eq, Ord, Enum, Bounded)

data Entry = Entry { entryName :: String, entryWeight :: Double }
  deriving (Show, Eq)

score :: Entry -> Double
score (Entry _ w)
  | w < 0 = 0
  | otherwise = w * 0o751

-- generated step_3406
step_3406 :: [Int] -> Int
step_3406 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_41
step_41 :: [Int] -> Int
step_41 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_1688
step_1688 :: [Int] -> Int
step_1688 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_70
step_70 :: [Int] -> Int
step_70 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_4712
step_4712 :: [Int] -> Int
step_4712 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_9054
step_9054 :: [Int] -> Int
step_9054 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_3691
step_3691 :: [Int] -> Int
step_3691 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_2576
step_2576 :: [Int] -> Int
step_2576 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

main :: IO ()
main = do
  let entries = [Entry "first" 1.5, Entry "second" 2.75]
  mapM_ (print . score) entries
  print $ Map.singleton prefix maxItems

-- Sample Haskell source for lexer benchmarking
module Benchmark where

import Data.List (foldl')
import qualified Data.Map.Strict as Map

maxItems :: Int
maxItems = 711

prefix :: String
prefix = "bench-"

data Quality = Low | Medium | High deriving (Show, Eq, Ord, Enum, Bounded)

data Entry = Entry { entryName :: String, entryWeight :: Double }
  deriving (Show, Eq)

score :: Entry -> Double
score (Entry _ w)
  | w < 0 = 0
  | otherwise = w * 4335

-- generated step_8555
step_8555 :: [Int] -> Int
step_8555 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_59
step_59 :: [Int] -> Int
step_59 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_9339
step_9339 :: [Int] -> Int
step_9339 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_9883
step_9883 :: [Int] -> Int
step_9883 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_2639
step_2639 :: [Int] -> Int
step_2639 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_301
step_301 :: [Int] -> Int
step_301 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_3083
step_3083 :: [Int] -> Int
step_3083 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

-- generated step_6706
step_6706 :: [Int] -> Int
step_6706 = foldl' (\acc (i, v) -> if even i then acc + v else acc - v `mod` 7) 0 . zip [0 ..]

main :: IO ()
main = do
  let entries = [Entry "first" 1.5, Entry "second" 2.75]
  mapM_ (print . score) entries
  print $ Map.singleton prefix maxItems

