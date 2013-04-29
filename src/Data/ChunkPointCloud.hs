{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.ChunkPointCloud where

import qualified Data.Point        as P
import qualified Data.Trees.KdTree as K


data ChunkPointCloud c = ChunkPointCloud { chunks :: K.KdTree c
                                         , critDist2 :: Double
                                         }

-- |Chunks all have location (point) and weight
class Chunk p cp | p -> cp where
  mkChunk  :: p -> Double -> cp
  location :: cp -> p
  weight   :: cp -> Double

{-
-}

-- |A simple chunk type carrying no special data
data BasicChunk p = BasicChunk { basicLoc :: p
                               , basicWeight :: Double
                               }

-- |A BasicChunk is a point because it has a location
instance  (Ord e, Num e, P.Point p e) => P.Point (BasicChunk p) e  where
  dimensions c = P.dimensions (location c)
  element n c  = P.element n (location c)
  dist2   a b  = P.dist2 (location a) (location b)

--instance (P.Point p e, P.Point p2 e) => Chunk (BasicChunk p) p2 where
instance Chunk p (BasicChunk p) where
  mkChunk pos w = BasicChunk { basicLoc=pos, basicWeight=w }
  location c    = basicLoc c
  weight   c    = basicWeight c



replaceChunkInCloud :: (Ord e, Num e, P.Point p e, Chunk p cp) => cp -> cp -> ChunkPointCloud cp -> ChunkPointCloud cp
replaceChunkInCloud oldP newP pCloud = 
  pCloud {chunks = newChunks}
    where
      oldChunks :: K.KdTree cp
      oldChunks = chunks pCloud
      newChunks :: K.KdTree cp
      newChunks = K.addPoint newP (K.remove oldP oldChunks)
      
integratePointIntoChunk :: (P.Point p e, Chunk p cp) =>  p -> cp -> cp
integratePointIntoChunk pnt chnk = mkChunk pnt 1.0
                         
{-
addPoint :: (P.Point p e, Chunk c p) => p -> ChunkPointCloud c -> ChunkPointCloud c
addPoint pnt pCloud
  | pDist2 < (critDist2 pCloud) = updatePoint nearestChunk modChunk pCloud
  | otherwise                   = K.addPoint (mkChunk pnt 1.0) pCloud
    where
      nearestChunk = K.nearestNeighbor pnt pCloud
      pDist2 = P.dist2 pnt nearestChunk
      modChunk = integratePointIntoChunk pnt nearestChunk
  -}                              
  
                                
                                
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
  dimensions _ p = 1
  element 0 p = p1x
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
 