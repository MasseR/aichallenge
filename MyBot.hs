module Main where

import Data.List
import Data.Maybe (mapMaybe, fromJust)
import System.IO
import qualified Data.PQueue.Min as PQ
import System.Random
import Control.Monad (msum)
import qualified Data.Map as M

import Ants

type Heuristics = Int
data Path = Path Heuristics Direction Point deriving (Eq, Ord)
type AntOrders = M.Map Ant [Order]

-- For now we don't need accurate distances
simpleDistance :: Point -> Point -> Int
simpleDistance (x1,y1) (x2,y2) = abs $ (x2-x1) + (y2-y1)

simpleClosest :: [Ant] -> Point -> Ant
simpleClosest ants dest = snd $ minimum $ map (\a -> (simpleDistance (pointAnt a) dest, a)) ants

path :: Point -> Point -> World -> [Direction]
path origin dest world = map (\(Path _ d _) -> d) $ go [] $ PQ.fromList $ open origin
  where
    open point = {-# SCC "open" #-} [Path (simpleDistance (move dir point) point) dir (move dir point) | dir <- [North .. West], walkable world (move dir point)]
    go :: [Path] -> PQ.MinQueue Path -> [Path]
    go path queue = {-# SCC "go" #-}
      let p@(Path _ _ point) = PQ.findMin queue
          newpath = p `seq` path ++ [p]
          newqueue = PQ.union (PQ.deleteMin queue) $ PQ.fromList $ open point
      in if point == dest then newpath else go newpath newqueue

-- | Picks the first "passable" order in a list
-- returns Nothing if no such order exists
tryOrder :: World -> [Order] -> Maybe Order
tryOrder w = find (passable w)

-- | Generates orders for an Ant in all directions
generateOrders :: Ant -> [Order]
generateOrders a = map (Order a) [North .. West]

appendOrder :: Order -> AntOrders -> AntOrders
appendOrder o o' = M.insertWith' (flip (++)) (ant o) [o] o'

toAntOrders :: [Order] -> AntOrders
toAntOrders = foldr appendOrder M.empty

gatherFoods :: GameState -> AntOrders
gatherFoods gs
  | length (food gs) > 0 = toAntOrders $ map (\f -> gatherFood (simpleClosest (myAnts $ ants gs) f) (world gs) f)  $ food gs
  | otherwise = M.empty

gatherFood :: Ant -> World -> Food -> Order
gatherFood ant w f = Order ant $ head $ path (pointAnt ant) f w

randomWalks :: StdGen -> GameState -> AntOrders
randomWalks g gs = toAntOrders $ fst $ foldr randomWalk ([], g) $ myAnts $ ants gs

walks :: GameState -> AntOrders
walks gs = toAntOrders [Order ant dir | ant <- myAnts $ ants gs, dir <- [North .. West]]

randomWalk :: Ant -> ([Order], StdGen) -> ([Order], StdGen)
randomWalk a (o, g) =
  let (i, g') = randomR (0,3) g
  in ((Order a ([North .. West] !! i)) : o, g')

genOrders :: StdGen -> GameState -> [Order]
genOrders gen gs = map head $ M.elems $ M.filter (\x -> length x > 0) $ M.map (filter (passable (world gs))) $ M.unionsWith (++) [
  gatherFoods gs
  , walks gs
  ]
-- genOrders stdgen gs = map head $ groupBy (\a b -> ant a == ant b) $ filter (passable (world gs)) $ concat [
--     gatherFoods gs
--     , randomWalks stdgen gs
--     , walks gs
--   -- , walks gs
--   ]

{- | 
 - Implement this function to create orders.
 - It uses the IO Monad so algorithms can call timeRemaining.
 -
 - GameParams data holds values that are constant throughout the game
 - GameState holds data that changes between each turn
 - for each see Ants module for more information
 -}
doTurn :: GameParams -> GameState -> IO [Order]
doTurn gp gs = do
  stdgen <- getStdGen
  let orders = genOrders stdgen gs
  r <- length orders `seq` timeRemaining gs
  hPutStrLn stderr $ show orders
  return orders

-- | This runs the game
main :: IO ()
main = game doTurn

-- vim: set expandtab:
