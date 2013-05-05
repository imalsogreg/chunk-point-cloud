{-# LANGUAGE TypeFamilies #-}

module Data.ChunkPointCloud where

import Data.Maybe
import Data.Point
import qualified Data.Trees.KdTree as K


data ChunkPointCloud p = ChunkPointCloud { chunks :: K.KdTree p
                                          , critDist2 :: Double
                                          }
{-
-- |Chunks all have location (point) and weight
class Chunk c where
  type Centroid c
  location :: c -> Centroid c
  weight   :: c -> Double
-}

-- |A simple chunk type carrying no special data
data BasicChunk p = BasicChunk { basicLoc :: p
                               , basicWeight :: Double
                               }


-- |A (BasicChunk p) is a point because it has a location
instance (Ord p, Num p, Point p) => Point (BasicChunk p)  where
  
  type Elem (BasicChunk p) = p
  mkPoint =  id
--  dimensions c = P.dimensions (location c)
--  element n c  = P.element n (location c)
--  dist2   a b  = P.dist2 (location a) (location b)

{-
instance (P.Point p e) => Chunk (BasicChunk p) p where
  location c    = basicLoc c
  weight   c    = basicWeight c


-- Just seeing if I can get the type signature right
getCloudPoints :: ChunkPointCloud p -> K.KdTree p
getCloudPoints pCloud = chunks pCloud

-- |Replace a chunk (indexed by position) with a new one
replaceChunkInCloud :: (Chunk cp p) =>
                       cp -> 
                       cp -> 
                       (ChunkPointCloud cp) -> 
                       (ChunkPointCloud cp)
replaceChunkInCloud oldChunk newChunk pCloud = 
  pCloud {chunks = newChunks}
    where
      oldChunks = chunks pCloud
      newChunks :: K.KdTree cp
      newChunks = K.addPoint newChunk (K.remove oldChunks oldChunk)

--instance P.Point p Double


-- |Weighted mean location between two points
twoPointWeightedMean :: (RealFloat e, P.Point p e) => p -> e -> p -> e -> p
twoPointWeightedMean p1 w1 p2 w2 = 
  P.mkPoint $ map wMean2 [0 .. (P.dimensions p1 - 1)]
    where
      wMean2 i = (P.element i p1) * w1 + (P.element i p2) * w2

-- |Modify a chunk by adding a single point to it
integratePointIntoChunk :: (RealFloat e, P.Point p e, P.Point p Double, Chunk cp p) =>  
                           p -> 
                           cp -> 
                           cp
integratePointIntoChunk pnt chnk = mkChunk newLoc newWeight
  where
    newLoc = twoPointWeightedMean (location chnk) (weight chnk) pnt (fromIntegral 1)
    newWeight = (weight chnk) + 1.0
    
                         
addPointToCloud :: (P.Point p e, Chunk cp p) => 
                   p -> 
                   ChunkPointCloud cp -> 
                   ChunkPointCloud cp
addPointToCloud pnt pCloud
  | pDist2 < (critDist2 pCloud) = cloudPointMerged
  | otherwise                   = cloudPointAdded
    where
      cloudPointMerged :: (Chunk cp p) => ChunkPointCloud cp
      cloudPointMerged = replaceChunkInCloud nearestChunk modChunk pCloud
      cloudPointAdded  = pCloud {chunks = K.addPoint loneChunk oldChunks}
      loneChunk = mkChunk pnt 1.0
      oldChunks = chunks pCloud
      nearestChunk :: (Chunk cp p) => cp
      nearestChunk = fromJust $ K.nearestNeighbor pnt pCloud
      pDist2 = P.dist2 pnt (location nearestChunk)
      modChunk = integratePointIntoChunk pnt nearestChunk

  
                                

data Point1d = Point1d { p1x :: Double }                         
               deriving (Eq, Ord, Show)

data Point2d = Point2d { p2x :: Double, p2y :: Double }
               deriving (Eq, Ord, Show)
                        
data Point3d = Point3d { p3x :: Double, p3y :: Double, p3z :: Double }
               deriving (Eq, Ord, Show)
                        
data Point4d = Point4d { p4a :: Double, p4b :: Double
                       , p4c :: Double, p4d :: Double }
               deriving (Eq, Ord, Show)
                        
data Point8d = Point8d { p8a :: Double, p8b :: Double
                       , p8c :: Double, p8d :: Double
                       , p8e :: Double, p8f :: Double
                       , p8g :: Double, p8h :: Double }
               deriving (Eq, Ord, Show)

instance P.Point Point1d Double where
  dimensions _ = 1
  element 0 p = p1x p
  dist2 a b = P.diff2 a b 0

instance P.Point Point2d Double where
  dimensions _ p = 2
  element 0 = p2x
  element 1 = p2y
  dist2 a b = sum $ map (P.diff2 a b) [0 .. P.dimensions a - 1]
                        
instance P.Point Point3d Double where
  dimensions _ p = 3
  element 0 = p3x
  element 1 = p3y
  element 2 = p3z
  dist2 a b = sum $ map (P.diff2 a b) [0 .. P.dimensions a - 1]
              
instance P.Point Point4d Double where
  dimensions _ p = 4
  element 0 = p4a
  element 1 = p4b
  element 2 = p4c
  element 3 = p4d
  dist2 a b = sum $ map (P.diff2 a b) [0 .. P.dimensions a - 1]
  
instance P.Point Point8d Double where
  dimensions _ p = 8
  element 0 = p8a
  element 1 = p8a
  element 2 = p8a
  element 3 = p8a
  element 4 = p8a
  element 5 = p8a
  element 6 = p8a
  element 7 = p8a
  dist2 a b = sum $ map (P.diff2 a b) [0 .. P.dimensions a - 1]
 -}