module Tortoise where

-- COMP3141 22T2 ASSIGNMENT 1

import Data.Semigroup
import Data.List
import Test.QuickCheck

-- data type definitions

data Freq = Freq Int deriving (Show, Eq, Ord)
data Interval = Interval Int deriving (Show, Eq, Ord)

type Count = Integer
data Histogram = Histogram [(Interval, Count)] deriving (Show, Eq)

data SigCard =
  SigCard {
    refHistogram :: Histogram,
    excluded :: [Interval]
  } deriving (Show, Eq)

data Verdict = RealWeapon | Dud deriving (Show, Eq)

-- helper functions

notImpl :: String -> a
notImpl x = error $ "'" ++ x ++ "'" ++ " not defined"

startPoint :: Interval -> Freq
startPoint (Interval x) = Freq (100*x)

endPoint :: Interval -> Freq
endPoint (Interval x) = Freq (100*x + 100)

-- ASSIGNMENT STARTS HERE --

-- Problem 1

inside :: Freq -> Interval -> Bool
x `inside` r = notImpl "inside"

intervalOf :: Freq -> Interval
intervalOf = notImpl "intervalOf"

prop_inIntervalOf :: Freq -> Bool
prop_inIntervalOf = notImpl "prop_inIntervalOf"

prop_inOneInterval :: Freq -> Interval -> Property
prop_inOneInterval = notImpl "prop_inOneInterval"

-- Problem 2

histogram :: [(Interval, Count)] -> Histogram
histogram = notImpl "histogram"

prop_histogram1 :: Histogram -> Bool
prop_histogram1 = notImpl "prop_histogram1"

prop_histogram2 :: Histogram -> Bool
prop_histogram2 = notImpl "prop_histogram2"

prop_histogram3 :: Histogram -> Bool
prop_histogram3 = notImpl "prop_histogram3"

-- Problem 3

process :: [Freq] -> Histogram
process = notImpl "process"

merge :: Histogram -> Histogram -> Histogram
merge = notImpl "merge"

prop_mergeAssoc :: Histogram -> Histogram -> Histogram -> Bool
prop_mergeAssoc = notImpl "prop_mergeAssoc"

prop_mergeId :: Histogram -> Bool
prop_mergeId = notImpl "prop_mergeId"

prop_mergeComm :: Histogram -> Histogram -> Bool
prop_mergeComm = notImpl "prop_mergeComm"

instance Semigroup Histogram where
  (<>) = notImpl "<> for Histogram"
instance Monoid Histogram where
  mappend = notImpl "mappend for Histogram"
  mempty = notImpl "mempty for Histogram"

-- Problem 4

report_refl :: Maybe Histogram
report_refl = notImpl "report_refl"

report_symm :: Maybe (Histogram, Histogram)
report_symm = notImpl "report_symm"

report_tran :: Maybe (Histogram, Histogram, Histogram)
report_tran = notImpl "report_tran"

-- Inspector O'Hare implemented match as follows:
match :: Histogram -> SigCard -> Verdict
match (Histogram h) (SigCard (Histogram r) v) =
  if d < 32 then RealWeapon else Dud where
    grab r (Histogram hs) = case filter (\x -> fst x == r) hs of
      [(_,x)] -> x
      _       -> 0
    squareDist (Histogram h1) (Histogram h2) = sum squares where
      common = sort . nub $ map fst h1 ++ map fst h2
      squares =
        map (\x -> (fromIntegral $ grab x (Histogram h1) - grab x (Histogram h2))**2)
            common
    d1 = squareDist (Histogram h) (Histogram r)
    h' = Histogram $ filter (\x -> fst x `elem` v) h
    r' = Histogram $ filter (\x -> fst x `elem` v) r
    d2 = squareDist h' r'
    d = sqrt (d1 - d2)

-- Use this reference card to find a false positive for `match`
refCard :: SigCard
refCard = SigCard (histogram r) v where
  r = [(Interval 4, 4000), (Interval 5, 6000), (Interval 6,300)]
  v = [Interval 5]

falsePos :: Histogram
falsePos = notImpl "falsePos"
