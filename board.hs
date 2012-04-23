module Board
( Area(..)
, Position
, Board           (..) -- data constructors for debug.

, startingBoard
, sizeX
, sizeY
, validPositions

, refer
, fillBoard
, flappableAreas
, reversal
, putableAreas
, countBW
, nextTrunColor
) 
where

import qualified Data.Vector    as VCT 
    ( Vector, (//), (!), replicate, filter, length, unstablePartition)

sizeX = 8       :: Int
sizeY = sizeX   :: Int

--------------------------------------------------------------------------------
--  ”Õ–Ê‚Ì\¬—v‘f
--------------------------------------------------------------------------------
data Area   = Space | Black | White | OB    deriving (Eq, Show)
reversal :: Area -> Area
reversal Black   =  White
reversal White   =  Black
reversal a       =  a

isDisk      :: Area -> Bool
isDisk a    =  a /= reversal a

type Position   = (Int,Int)
validPositions  = [(x,y) | x <- [0..sizeX-1], y <- [0..sizeY-1]] :: [Position]

type Index  = Int
toIndex         :: Position -> Index
toIndex (x, y)  =  x + 2 + (y * (sizeX + 1)) + sizeX

toPosition      :: Index -> Position
toPosition ix   =  (x - 1, y)
  where (y, x)  = quotRem (ix - sizeX - 1) (sizeX + 1)

type Direction  = Index
upperLeft   = -sizeX - 2                            :: Direction
up          = -sizeX - 1                            :: Direction
upperRight  = -sizeX                                :: Direction
right       = 1                                     :: Direction
lowerRight  = sizeX + 2                             :: Direction
low         = sizeX + 1                             :: Direction
lowerLeft   = sizeX                                 :: Direction
left        = -1                                    :: Direction
directions  = [ upperLeft, up, upperRight, right,
                lowerRight, low, lowerLeft, left ]  :: [Direction]

neighbor    =  (+)                  :: Index -> Direction -> Index
--------------------------------------------------------------------------------
--  ”Õ–Ê
--------------------------------------------------------------------------------
newtype Board = Board (VCT.Vector Area)

startingBoard   :: Board
startingBoard   =  Board $ allOB VCT.// (spaces ++ diskes)
  where
    allOB               =  VCT.replicate vectorSize OB
      where vectorSize  = (sizeX + 1) * (sizeY + 2) + 1
    spaces  = zip (map toIndex validPositions) (repeat Space)
    diskes  = zip centers (iterate reversal White)
    centers = center : map (neighbor center) [left, upperLeft, up]
    center  = toIndex (sizeX `div` 2, sizeY `div` 2)

--------------------------------------------------------------------------------
refer           :: Board -> Position -> Area
refer b         =  (b @@) . toIndex
(@@)            :: Board -> Index -> Area
(@@) (Board b)  =  (b VCT.!)

fillBoard                   :: [Position] -> Area -> Board -> Board
fillBoard ps a (Board b)    =  Board $ b VCT.// targetList
  where targetList  = zip (map toIndex ps) (repeat a)

flappableAreas :: Board -> Area -> Position -> [Position]
flappableAreas board color putPosition
 | Space /= refer board putPosition = []
 | otherwise                        = concatMap (map toPosition . f) directions
  where
   f d = checkOneWay (toIndex putPosition `neighbor` d) d []
    where
     checkOneWay :: Index -> Direction -> [Index] -> [Index]
     checkOneWay p d acc
      | board @@ p == color             = acc
      | board @@ p == reversal color    = checkOneWay (neighbor p d) d (p:acc)
      | otherwise                       = []

putableAreas         :: Area -> Board -> [Position]
putableAreas color b =  filter (not.null.flappableAreas b color) validPositions

countBW                 :: Board -> (Int, Int)
countBW (Board b)       =  double VCT.length $ blackWhite b
  where double f (x, y) =  (f x, f y)
        blackWhite      =  VCT.unstablePartition (== Black) . VCT.filter isDisk

nextTrunColor   :: Board -> Area -> Area
nextTrunColor curtBoard curtColor
 | isPutable (reversal curtColor)   = reversal curtColor
 | isPutable curtColor              = curtColor
 | otherwise                        = Space
    where isPutable color = putableAreas color curtBoard /= []