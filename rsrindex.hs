{-# LANGUAGE ConstraintKinds #-}
module RSRIndex (
    calcIndex
)
where

import Data.List
import Data.Time
import Data.Time.Format
import Statistics.Regression
import Statistics.Matrix.Types
import qualified Data.Vector.Unboxed as U
import qualified Data.Map as M

--convenient alias
type RSRIndex = (Vector, Double)

class (Eq a, Show a, Ord a) => AssetSale a where
    date :: a -> UTCTime
    price :: a -> Int
    id :: Asset b => a -> b 

class (Eq a, Show a, Ord a) => Asset a where
    self :: a -> a

class SalePairSet a where 
    pairs :: AssetSale b => a -> [(b,  b)]
    unmatched :: (Asset b, AssetSale c) => a -> M.Map b c

--returns an RSR Index normalized to 100
calcIndex :: [[Int]] -> [Double]-> RSRIndex
calcIndex priceMatrix logVector = normalizeIndex $ doRegression priceMatrix logVector 

--adapter to the olsRegress function - converting types
--for some reason the matrix is represented by a list of columns instead of the expected rows
doRegression :: [[Int]] -> [Double] -> RSRIndex
doRegression priceMatrix logVector = olsRegress ([(U.fromList ([fromIntegral i | i <- l])) | l <- (transpose priceMatrix)]) (U.fromList logVector)

normalizeIndex :: RSRIndex -> RSRIndex
normalizeIndex (index, fit) = (U.fromList [exp (i - (index U.! 0) + log 100) | i <- U.toList index], fit)