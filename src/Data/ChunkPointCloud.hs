module Data.ChunkPointCloud where

import qualified Data.Point as P
import qualified Data.Trees.KdTree as K

data ChunkPointCloud p = ChunkPointCloud { chunks :: K.KdTree p
                                         , critDist2 :: Double
                                         }

class 

data P.Point p => Chunk p = Chunk { loc :: p
                                  , weight :: IntegerRR

updatePoint :: P.Point p => p -> p -> ChunkPointCloud -> ChunkPointCloud
updatePoint oldP newP = K.addPoint newP . K.remove oldP
  
integratePointToChunk
                         
addPoint :: P.Point p => p -> ChunkPointCloud -> ChunkPointCloud
addPoint pnt pCloud
  | pDist2 < (critDist2 cld)  = K.addPoint $ K.remove
  
                                
                                
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
 