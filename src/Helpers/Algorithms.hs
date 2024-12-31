module Helpers.Algorithms
  ( dijkstra,
    bfs,
  )
where

import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.PQueue.Min qualified as PQ

dijkstra :: (Ord s) => s -> (s -> [(Int, s)]) -> (s -> Bool) -> (Maybe (Int, s), M.Map s Int)
dijkstra start nbf isGoal = (PQ.getMin finQ, finF)
  where
    (finQ, finF) = until isDone dijkstraStep (initQ, M.empty)
    getNext queue found = do
      ((dist, m), q) <- PQ.minView queue
      if M.notMember m found
        then Just ((dist, m), q, M.insert m dist found)
        else getNext q found
    dijkstraStep (queue, found) = fromMaybe (PQ.empty, found) $ do
      ((dist, m), queue', found') <- getNext queue found
      let newNbs = [(dist + edgeDist, nb) | (edgeDist, nb) <- nbf m, M.notMember nb found']
      pure (foldr PQ.insert queue' newNbs, found')

    isDone (queue, _) = PQ.null queue || (isGoal $ snd $ PQ.findMin queue)

    initQ = PQ.insert (0, start) PQ.empty

bfs :: (Ord s) => s -> (s -> [s]) -> M.Map s Int
bfs start nbf = bfs' 0 (M.singleton start 0) [start]
  where
    bfs' _ found [] = found
    bfs' dist found starts = bfs' (dist + 1) found' extraNbs
      where
        (extraNbs, found') =
          foldr
            ( \nb (nbl, fd) ->
                if nb `M.member` fd
                  then (nbl, fd)
                  else (nb : nbl, M.insert nb (dist + 1) fd)
            )
            ([], found)
            $ starts >>= nbf
