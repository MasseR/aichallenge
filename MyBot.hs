module Main where

import Data.List
import Data.Maybe (mapMaybe, fromJust)
import System.IO
import qualified Data.PQueue.Min as PQ
import System.Random
import Control.Monad (msum)

import Ants

type Heuristics = Int
data Path = Path Heuristics Direction Point deriving (Eq, Ord)

-- For now we don't need accurate distances
simpleDistance :: Point -> Point -> Int
simpleDistance (x1,y1) (x2,y2) = abs $ (x2-x1) + (y2-y1)

simpleClosest :: [Ant] -> Point -> Ant
simpleClosest ants dest = snd $ minimum $ map (\a -> (simpleDistance (pointAnt a) dest, a)) ants

path :: Point -> Point -> World -> [Direction]
path origin dest world = map (\(Path _ d _) -> d) $ go [] $ PQ.fromList $ open origin
  where
    open point = [Path (simpleDistance (move dir point) point) dir (move dir point) | dir <- [North .. West], walkable world (move dir point)]
    go :: [Path] -> PQ.MinQueue Path -> [Path]
    go path queue =
      let p@(Path _ _ point) = PQ.findMin queue
          newpath = path ++ [p]
          newqueue = PQ.union (PQ.deleteMin queue) $ PQ.fromList $ open point
      in if point == dest then newpath else go newpath newqueue

-- | Picks the first "passable" order in a list
-- returns Nothing if no such order exists
tryOrder :: World -> [Order] -> Maybe Order
tryOrder w = find (passable w)

-- | Generates orders for an Ant in all directions
generateOrders :: Ant -> [Order]
generateOrders a = map (Order a) [North .. West]

gatherFoods :: GameState -> Maybe [Order]
gatherFoods gs
  | length (food gs) > 0 = Just $ map (\f -> gatherFood (simpleClosest (myAnts $ ants gs) f) (world gs) f)  $ food gs
  | otherwise = Nothing

gatherFood :: Ant -> World -> Food -> Order
gatherFood ant w f = Order ant $ head $ path (pointAnt ant) f w

randomWalks :: StdGen -> GameState -> Maybe [Order]
randomWalks g gs = Just $ fst $ foldr randomWalk ([], g) $ myAnts $ ants gs

randomWalk :: Ant -> ([Order], StdGen) -> ([Order], StdGen)
randomWalk a (o, g) =
  let (i, g') = randomR (0,3) g
  in ((Order a ([North .. West] !! i)) : o, g')

genOrders :: StdGen -> GameState -> [Order]
genOrders stdgen gs = fromJust $ msum [
    gatherFoods gs
  , randomWalks stdgen gs
  ]

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
  -- generate orders for all ants belonging to me
  -- let generatedOrders = map generateOrders $ myAnts $ ants gs
  -- -- for each ant take the first "passable" order, if one exists
  --     orders = mapMaybe (tryOrder (world gs)) generatedOrders
  -- -- this shows how to check the remaining time
  let orders = genOrders stdgen gs
  elapsedTime <- timeRemaining gs
  hPutStrLn stderr $ show elapsedTime
  -- wrap list of orders back into a monad
  return orders

-- | This runs the game
main :: IO ()
main = game doTurn

-- vim: set expandtab:
