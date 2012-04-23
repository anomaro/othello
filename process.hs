module Process
( putDisk
) where

import Board    ( Area(..), Position, reversal, fillBoard, flappableAreas
                , putableAreas, countBW, nextTrunColor)
import State    (GameState, Input(Input), assign, readComponent)
import qualified Com.A              as A

import qualified GameTree   as G

import Control.Monad ((>=>))


putDisk :: GameState -> IO()
putDisk gs  = do
    nextColor   <- readComponent gs
    p           <- identifyPlayer nextColor $ gs
    putDisk' nextColor p
  where
    putDisk' :: Area -> Maybe Position -> IO ()
    putDisk' _         Nothing     = return ()
    putDisk' nextColor (Just p)    = do
        curtBoard   <- readComponent gs
        renewBoardState (flappableAreas curtBoard nextColor p) curtBoard
        readComponent gs >>= print . countBW
        deleteInput
      where
        renewBoardState [] _     = return ()
        renewBoardState ps b     = assign gs newBoard >> assign gs newColor
          where newBoard    = fillBoard (p:ps) nextColor b
                newColor    = nextTrunColor newBoard nextColor
--                 | isPutable (reversal nextColor)   = reversal nextColor
--                 | isPutable nextColor              = nextColor
--                 | otherwise                        = Space
--                  where isPutable color = putableAreas color newBoard /= []
    deleteInput = assign gs (Input Nothing)

type DecisionMaker  = GameState -> IO (Maybe Position)

identifyPlayer          :: Area -> DecisionMaker
identifyPlayer Black    =  com
identifyPlayer White    =  comTest
identifyPlayer _        =  gameEnd

user    :: DecisionMaker
user    =  readComponent >=> (\(Input p) -> return p)

deleteInput :: GameState -> IO()
deleteInput gs = assign gs (Input Nothing)

com     :: DecisionMaker
com     =  A.randomDecision
--com gs  = do
--    curtBoard   <- readComponent gs
--    nextColor   <- readComponent gs
--    return $ Just $ A.decisionMaking nextColor curtBoard

comTest :: DecisionMaker
comTest gs  = do
    curtBoard   <- readComponent gs
    nextColor   <- readComponent gs
    return $ Just $ G.decisionMaking nextColor curtBoard

gameEnd     :: DecisionMaker
gameEnd gs  =  return Nothing