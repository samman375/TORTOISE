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
eucDistance g h = calcDistance $ getDistances (allIntervals g h) g h
 where
  allIntervals :: Histogram -> Histogram -> [Interval]
  allIntervals a b = nub $ sort $ map (\xs -> fst xs) (deconstructHist a ++ deconstructHist b)
  getDistances :: [Interval] -> Histogram -> Histogram -> [Int]
  getDistances xs a b = map (\i -> countDifference i a b) xs
   where
    countDifference :: Interval -> Histogram -> Histogram -> Int
    countDifference i a b = (getCount i (deconstructHist a)) - (getCount i (deconstructHist b))
    getCount :: Interval -> [(Interval, Count)] -> Int
    getCount i []         = 0
    getCount i ((a,x):xs) = if a == i then fromIntegral x else getCount i xs
  calcDistance :: [Int] -> Float
  calcDistance xs = sqrt $ fromIntegral (sum $ map (^2) xs)

testHistogram1 :: Histogram
testHistogram1 = (histogram [(interval 1, 5), (interval 2, 4)])

testHistogram2 :: Histogram
testHistogram2 = (histogram [(interval 1, 2)])

eucTest :: Bool
eucTest = eucDistance testHistogram1 testHistogram2 == 5

prop_refl :: Histogram -> Bool
prop_refl g = eucDistance g g == eucDistance g g

prop_symm :: Histogram -> Histogram -> Bool
prop_symm g h = eucDistance g h == eucDistance h g

prop_tran :: Histogram -> Histogram -> Histogram -> Property
prop_tran a b c = eucDistance a b == eucDistance b c ==> eucDistance a c == eucDistance a b

report_refl :: Maybe Histogram
report_refl = Nothing

report_symm :: Maybe (Histogram, Histogram)
report_symm = Nothing

report_tran :: Maybe (Histogram, Histogram, Histogram)
report_tran = Just (histogram [], histogram [(interval 1, 1)], histogram [])

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
falsePos = histogram [(Interval 4, 3000), (Interval 5, 60000000000000), (Interval 6, 300)]
