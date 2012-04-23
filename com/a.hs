module Com.A
where

import Board (Board, Area, Position, putableAreas)
import State (GameState, readComponent)
import qualified System.Random.Shuffle  as RNS (shuffleM)

decisionMaking          :: Area -> Board -> Position
decisionMaking color    =  head . putableAreas color

randomDecision      :: GameState -> IO (Maybe Position)
randomDecision gs   =  do
    curtBoard       <- readComponent gs
    nextColor       <- readComponent gs
    putPositions    <- RNS.shuffleM $ putableAreas nextColor curtBoard
    return $ Just $ head putPositions