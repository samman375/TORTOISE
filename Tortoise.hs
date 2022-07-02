module Tortoise where

-- COMP3141 22T2 ASSIGNMENT 1

import Data.Function (on)
import Data.List
import Data.Semigroup
import Test.QuickCheck

-- data type definitions

data Freq = Freq Int deriving (Show, Eq, Ord)
data Interval = Interval Int deriving (Eq, Ord)

instance Show Interval where
  show a = show (extractFreq $ startPoint a) ++ " to " ++ show (extractFreq $ endPoint a)

type Count = Integer
data Histogram = Histogram [(Interval, Count)] deriving (Show, Eq)

data SigCard = SigCard
  { refHistogram :: Histogram
  , excluded :: [Interval]
  }
  deriving (Show, Eq)

data Verdict = RealWeapon | Dud deriving (Show, Eq)

instance Arbitrary Freq where
  -- arbitrary = freq <$> (arbitrary :: Gen (Large Int))
  arbitrary = freq <$> arbitrary

instance Arbitrary Interval where
  -- arbitrary = interval <$> (arbitrary :: Gen (Large Int)) -- Large 3141
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

extractFreq :: Freq -> Int
extractFreq (Freq n) = n

deconstructHist :: Histogram -> [(Interval, Count)]
deconstructHist (Histogram h) = h

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
prop_inOneInterval x y = y /= intervalOf x ==> x `inside` y == False

-- Problem 2

histogram :: [(Interval, Count)] -> Histogram
histogram xs = Histogram $ sortIntervals $ removeDups $ removeNones xs
 where
  sortIntervals       = sortBy (compare `on` fst)
  removeNones         = filter (\t -> (snd t) > 0)
  removeDups []       = []
  removeDups (x : xs) = x : removeDups (filter (\t -> (fst t) /= (fst x)) xs)

-- every interval occur at most once
prop_histogram1 :: Histogram -> Bool
prop_histogram1 (Histogram xs) = length xs == length (nub $ map fst xs)

-- interval not included if count <= 0
prop_histogram2 :: Histogram -> Bool
prop_histogram2 (Histogram xs) = all (\t -> (snd t) > 0) xs

-- intervals sorted
prop_histogram3 :: Histogram -> Bool
prop_histogram3 (Histogram xs) = sort (map fst xs) == map fst xs

-- Problem 3

process :: [Freq] -> Histogram
process xs = histogram $ countIntervals $ groupIntervals xs
 where
  groupIntervals ys = group $ sort $ map intervalOf ys
  countIntervals ys = map (\l -> (head l, toInteger $ length l)) ys

merge :: Histogram -> Histogram -> Histogram
merge xs ys = histogram $ map combineIntervals $ groupByInterval $ sortIntervals $ combineHists xs ys
 where
  combineHists g h = ((deconstructHist g) ++ (deconstructHist h))
  sortIntervals = sortBy (compare `on` fst)
  groupByInterval = groupBy (\a b -> fst a == fst b)
  combineIntervals l = (fst $ head l, sum $ map snd l)

prop_mergeAssoc :: Histogram -> Histogram -> Histogram -> Bool
prop_mergeAssoc g h i = merge g (merge h i) == merge (merge g h) i

prop_mergeId :: Histogram -> Bool
prop_mergeId g = merge g mempty == g

prop_mergeComm :: Histogram -> Histogram -> Bool
prop_mergeComm g h = merge g h == merge h g

prop_mergeGiven :: [Freq] -> [Freq] -> Bool
prop_mergeGiven xs ys = merge (process xs) (process ys) == process (xs ++ ys)

instance Semigroup Histogram where
  (<>) = merge
instance Monoid Histogram where
  mappend = (<>)
  mempty = histogram []

-- Problem 4

eucDistance :: Histogram -> Histogram -> Float
eucDistance g h = undefined
 where
  allIntervals a b = nub $ sort $ map (\xs -> fst xs) (deconstructHist a ++ deconstructHist b)
  -- [Interval] -> Histogram -> Histogram -> [Int]
  getDistances xs g h = map (\i -> countDifference i g h) xs
  -- [Int] -> Float
  calcDistance xs = sqrt (sum $ map (^2) xs)
  -- take list of intervals, take count for interv from hist 1 (else 0), take count for interv from hist 2 (else 0)
  countDifference i g h = (getCount i g) - (getCount i h)
  getCount i h 
    | i `elem` (map fst h) = snd $ (h !! (elemIndex i (map fst h)))
    | otherwise = 0

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
-- int overflow