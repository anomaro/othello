module GameTree
where

import Data.Tree (Tree(Node))
import Board
import Board_Debug 
import Data.Tree.Game_tree.Game_tree
import Data.Tree.Game_tree.Negascout

--type GameTree   = Tree (Board, Area)

--test = generate startingBoard Black 3

--generate    :: Board -> Area -> Int -> GameTree
--generate curtBoard curtColor depth
--    = Node (curtBoard, curtColor) (children curtColor depth)
--  where
--    children _     0 = []
--    children Space _ = []
--    children c     n = map f (putableAreas c curtBoard)
--      where
--        f :: Position -> GameTree
--        f p = generate nextBoard nextColor (depth - 1)
--          where
--            nextBoard = fillBoard fillAreas c curtBoard
--            fillAreas = p : flappableAreas curtBoard c p
--            nextColor = nextTrunColor nextBoard c

decisionMaking      :: Area -> Board -> Position
decisionMaking c b  =  extract $ alpha_beta_search starting depth
  where extract     =  readPutPosition . head . tail . fst
        starting    =  GamePosition b c (0,0)
        depth       =  4

test = alpha_beta_search (GamePosition startingBoard Black (1,1)) 3

data GamePosition = GamePosition 
                        { readBoard         :: Board
                        , readArea          :: Area
                        , readPutPosition   :: Position } deriving (Show)
instance Game_tree GamePosition where
    is_terminal (GamePosition _ Space _)    = True
    is_terminal _                           = False

    node_value  (GamePosition b c _)        = f c $ countBW b
      where f Black = fst
            f _     = snd

    children    (GamePosition b c p)        = map f (putableAreas c b)
      where f p'                = GamePosition nextBoard nextColor p'
              where nextBoard   = fillBoard fillAreas c b
                    fillAreas   = p' : flappableAreas b c p'
                    nextColor   = nextTrunColor nextBoard c