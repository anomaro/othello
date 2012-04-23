module State
( GameState
, Input (..)
, startingGameState
, assign
, readComponent
) where

import qualified Data.IORef         as REF
import Board (Board, Area(Black), Position, startingBoard)

--------------------------------------------------------------------------------
--  ó‘Ô
--------------------------------------------------------------------------------
type GameState      = REF.IORef GameState'
data GameState'     = GameState'
                        { readBoard         :: Board
                        , readNext          :: Area
                        , readInputMouse    :: Input
                        }
modify  :: (Board -> Board)
        -> (Area -> Area)
        -> (Input -> Input)
        -> GameState'
        -> GameState'
modify fb fa fim (GameState' b a im)    = GameState' (fb b) (fa a) (fim im)

startingGameState   :: IO GameState
startingGameState   =  REF.newIORef startingGameState'
  where startingGameState'  = GameState' startingBoard Black startingIM
        startingIM          = Input Nothing
       
--------------------------------------------------------------------------------
--  “ü—Í
--------------------------------------------------------------------------------
newtype Input  = Input (Maybe Position) deriving (Show)

--------------------------------------------------------------------------------
--  \¬—v‘f’PˆÊ‚Ì‘€ì
--------------------------------------------------------------------------------
class Component a where
    assign          :: GameState -> a -> IO ()
    readComponent   :: GameState -> IO a

instance Component Board where
    assign gs b     = REF.modifyIORef gs $ modify (const b) id id
    readComponent   = fmap readBoard . REF.readIORef

instance Component Area where
    assign gs a     = REF.modifyIORef gs $ modify id (const a) id
    readComponent   = fmap readNext . REF.readIORef

instance Component Input where
    assign gs im    = REF.modifyIORef gs $ modify id id (const im)
    readComponent   = fmap readInputMouse . REF.readIORef