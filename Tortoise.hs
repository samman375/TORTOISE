module Tortoise where

-- COMP3141 22T2 ASSIGNMENT 1

import Data.Function (on)
import Data.List
import Data.Semigroup
import Test.QuickCheck

-- data type definitions

data Freq = Freq Int deriving (Show, Eq, Ord)
data Interval = Interval Int deriving (Eq, Ord)

-- instance (Interval a) => Show a where
instance Show Interval where
  show a = show (startPoint a) ++ " to " ++ show (endPoint a)

type Count = Integer
data Histogram = Histogram [(Interval, Count)] deriving (Show, Eq) -- removed Show instance

data SigCard = SigCard
  { refHistogram :: Histogram
  , excluded :: [Interval]
  }
  deriving (Show, Eq) -- removed Show instance

data Verdict = RealWeapon | Dud deriving (Show, Eq)

instance Arbitrary Freq where
  -- arbitrary = freq <$> (arbitrary :: Gen (Large Int))
  arbitrary = freq <$> arbitrary

instance Arbitrary Interval where
  -- arbitrary = interval <$> (arbitrary :: Gen (Large Int))
  arbitrary = interval <$> arbitrary

instance Arbitrary Histogram where
  arbitrary = histogram <$> arbitrary

-- helper functions

notImpl :: String -> a
notImpl x = error $ "'" ++ x ++ "'" ++ " not defined"

startPoint :: Interval -> Freq
startPoint (Interval x) = Freq (100 * x)

endPoint :: Interval -> Freq
endPoint (Interval x) = Freq (100 * x + 100)

freq :: Int -> Freq
freq n = Freq (abs n)

interval :: Int -> Interval
interval n = Interval (abs n)

-- ASSIGNMENT STARTS HERE --

-- Problem 1

inside :: Freq -> Interval -> Bool
x `inside` r
  | x >= startPoint r && x < endPoint r = True
  | otherwise = False

intervalOf :: Freq -> Interval
intervalOf (Freq x) = Interval (x `div` 100)

prop_inIntervalOf :: Freq -> Bool
prop_inIntervalOf x = x `inside` (intervalOf x)

prop_inOneInterval :: Freq -> Interval -> Property
-- prop_inOneInterval x y = Interval z != y => not
prop_inOneInterval x y = y /= intervalOf x ==> x `inside` y == False

-- Problem 2

-- every interval occur only once
-- interval not included if count <= 0
-- intervals sorted
histogram :: [(Interval, Count)] -> Histogram
histogram xs = Histogram $ sortIntervals $ removeDups $ removeNones xs
 where
  removeNones ys = filter (\t -> (snd t) > 0) ys
  removeDups ys = nubFst ys
   where
    nubFst [] = []
    nubFst (x : xs) = x : nubFst (filter (\t -> (fst t) /= (fst x)) xs)
  sortIntervals ys = sortBy (compare `on` snd) ys

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
  if d < 32 then RealWeapon else Dud
 where
  grab r (Histogram hs) = case filter (\x -> fst x == r) hs of
    [(_, x)] -> x
    _ -> 0
  squareDist (Histogram h1) (Histogram h2) = sum squares
   where
    common = sort . nub $ map fst h1 ++ map fst h2
    squares =
      map
        (\x -> (fromIntegral $ grab x (Histogram h1) - grab x (Histogram h2)) ** 2)
        common
  d1 = squareDist (Histogram h) (Histogram r)
  h' = Histogram $ filter (\x -> fst x `elem` v) h
  r' = Histogram $ filter (\x -> fst x `elem` v) r
  d2 = squareDist h' r'
  d = sqrt (d1 - d2)

-- Use this reference card to find a false positive for `match`
refCard :: SigCard
refCard = SigCard (histogram r) v
 where
  r = [(Interval 4, 4000), (Interval 5, 6000), (Interval 6, 300)]
  v = [Interval 5]

falsePos :: Histogram
falsePos = notImpl "falsePos"
