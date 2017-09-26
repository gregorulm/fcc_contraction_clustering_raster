{-

Contraction Clustering (RASTER):
Reference Implementation in Haskell with an Example
(c) 2016, 2017 Fraunhofer-Chalmers Centre for Industrial Mathematics

Algorithm development and implementation:
Gregor Ulm (gregor.ulm@fcc.chalmers.se)

This demo has been developed using GHC 7.10.3 on Ubuntu Linux 16.04.

For a description of the algorithm including relevant theory, please
consult our paper on Contraction Clustering (RASTER).

-}

import System.IO
import Data.List.Split
import Control.Monad
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import qualified Data.List       as List

type Point      = (Double, Double)
type Point_Int  = (Int, Int)
type Map_Points = Map.Map (Int, Int) Int
type Set_Points = Set.Set Point_Int
type Output     = [(Int, Double, Double)]


all_points :: [Point] -> Map_Points -> Int -> Map_Points
all_points []          acc _      = acc
all_points ((x, y):xs) acc scalar = all_points xs acc' scalar
    where
        x'     = truncate $ x * fromIntegral scalar
        y'     = truncate $ y * fromIntegral scalar
        acc'   = case Map.lookup (x', y') acc of
                    Just val -> Map.insert (x', y') (val + 1) acc
                    Nothing  -> Map.insert (x', y')  1        acc


map_to_tiles :: [Point] -> Int -> Int -> ([Point_Int], Int)
map_to_tiles points precision threshold = (tiles, scalar)
    where
        scalar = 10 ^ precision
        ps     = all_points points Map.empty scalar
        tiles  = Map.keys $ Map.filter (>= threshold) ps


get_neighbors :: Point_Int -> Set_Points ->  Set_Points
get_neighbors (x, y) tiles =
    Set.filter (\x -> Set.member x tiles) neighbors
    where
        neighbors = Set.fromList [(x + 1, y    ),
                                  (x - 1, y    ),
                                  (x    , y + 1),
                                  (x    , y - 1),
                                  (x + 1, y - 1),
                                  (x + 1, y + 1),
                                  (x - 1, y - 1),
                                  (x - 1, y + 1)]


-- Clusters: list of sets, where each set represents a cluster
cluster_all :: Set_Points -> Int -> [Set_Points] -> [Set_Points]
cluster_all tiles min_size acc
    | Set.null tiles = acc
    | otherwise      = cluster_all ts' min_size acc'
        where
            start   = Set.singleton $ Set.elemAt 0 tiles
            cluster = cluster_one start tiles Set.empty
            ts'     = Set.difference tiles cluster
            acc'    = if Set.size cluster >= min_size
                        then cluster:acc
                        else acc


cluster_one :: Set_Points -> Set_Points -> Set_Points -> Set_Points
cluster_one to_check tiles acc
    | Set.null to_check = acc
    | otherwise         = cluster_one (Set.union xs vals) tiles acc'
        where
            x    = Set.elemAt 0 to_check
            xs   = Set.difference to_check $ Set.singleton x
            acc' = Set.insert x acc
            pred = \x -> not $ Set.member x acc'
            vals = Set.filter pred $ get_neighbors x tiles


number_clusters :: [Set_Points] -> Output -> Int -> Int -> Output
number_clusters []     acc scalar _ = acc
number_clusters (c:cs) acc scalar i =
    number_clusters cs acc' scalar (i + 1)
    where
        numbered = List.map (\x -> scale x scalar i) $ Set.toList c
        acc'     = numbered ++ acc


scale :: (Int, Int) -> Int -> Int -> (Int, Double, Double)
scale (x, y) scalar i = (i, x' / scalar', y' / scalar')
    where
        scalar'  = fromIntegral scalar
        x'       = fromIntegral x
        y'       = fromIntegral y


make_tuple :: String -> (Double, Double)
make_tuple line = (a', b')
    where
        (a:b:_) = splitOn "," line
        a'      = read a :: Double
        b'      = read b :: Double


prepare_output []               acc = concat acc
prepare_output ((num, x, y):xs) acc = prepare_output xs (a:acc)
    where
        a = show num ++ ", " ++ show x ++ ", " ++ show y ++ "\n"


main :: IO ()
main = do

    handle <- openFile "input/sample.csv" ReadMode

    let contents :: IO String
        contents = hGetContents handle

    lines <- fmap (splitOn "\n") contents

    let res = map make_tuple lines

        -- step 1: projection
        threshold = 5
        precision = 1
        -- tiles: significant tiles
        (tiles, scalar) = map_to_tiles res precision threshold

        -- step 2: agglomeration
        min_size = 5
        clusters = cluster_all (Set.fromList tiles) min_size []
        numbered = number_clusters clusters [] scalar 1

    putStrLn $ "Number of clusters: " ++ show (length clusters)
    
    {-
    mapM (\(x, y, z) -> putStrLn $
        show x ++ ", " ++ show y ++ ", " ++ show z ) numbered
    -}
    
    let output = prepare_output numbered []
        header = "Cluster Number, X-Position, Y-Position\n"
    
    writeFile "output/clustered.csv" $ header ++ output

    hClose handle
