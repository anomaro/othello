module Board_Debug 
where

import Board
import qualified Data.Vector    as VCT (toList)

instance Show Board where
    show (Board vec) = map toChar (format $ VCT.toList vec) 
      where format       = drop (sizeX+1) . take ((sizeX+1)*(sizeY+1)+1)
            toChar Space = '-'
            toChar Black = '*'
            toChar White = '+'
            toChar OB    = '\n'
