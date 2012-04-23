module Main
where

import qualified Graphics.UI.GLUT   as GLUT
import qualified System.Exit        as EXIT (ExitCode(ExitSuccess), exitWith)
import Board (Board, Area(..), Position, sizeX, sizeY, validPositions, refer)
import State (GameState, Input(Input), startingGameState, readComponent, assign)
import Process (putDisk)

main :: IO()
main =  do
   
    board <- startingGameState
   
    GLUT.initialDisplayMode GLUT.$= [GLUT.RGBMode, GLUT.DoubleBuffered]
    GLUT.initialWindowSize  GLUT.$= GLUT.Size windowSizeX windowSizeY
   
    GLUT.createWindow "othello"
   
    GLUT.displayCallback    GLUT.$= display board
    GLUT.keyboardMouseCallback  GLUT.$= Just (keyboardMouse board)
    GLUT.addTimerCallback timerInterval $ timer (putDisk board >> display board)
   
    GLUT.mainLoop

--------------------------------------------------------------------------------
--  コールバック関数
--------------------------------------------------------------------------------
display     :: GameState -> IO()
display bs  =  do
    GLUT.clear [GLUT.ColorBuffer]

    curtBoard   <- readComponent bs
    renderBoard curtBoard
   
    GLUT.swapBuffers

timer       :: IO() -> IO()
timer act   =  do
    act
    GLUT.addTimerCallback timerInterval $ timer act

-- Key -> KeyState -> Modifiers -> Position -> IO ()
keyboardMouse   :: GameState
                -> GLUT.Key -> GLUT.KeyState -> t -> GLUT.Position -> IO ()
keyboardMouse bs k s _ (GLUT.Position x y) 
  | k == GLUT.Char 'q'                       = EXIT.exitWith EXIT.ExitSuccess
  | k == GLUT.MouseButton GLUT.LeftButton && s == GLUT.Up   = assign bs input
  | otherwise                                               = return ()
  where
    input   = Input $ Just (fromEnum x `div` pixelX, fromEnum y `div` pixelY)

--------------------------------------------------------------------------------
--  設定
--------------------------------------------------------------------------------
timerInterval = 100   :: GLUT.Timeout
windowSizeX   = 640   :: GLUT.GLsizei
windowSizeY   = 640   :: GLUT.GLsizei
aspectRatio   = fromIntegral windowSizeX / fromIntegral windowSizeY  :: Double

unitAreaX   = 1.0 / fromIntegral sizeX          :: Double
unitAreaY   = unitAreaX * aspectRatio           :: Double

pixelX      = fromEnum windowSizeX `div` sizeX    :: Int
pixelY      = fromEnum windowSizeY `div` sizeY    :: Int
--------------------------------------------------------------------------------
--  描画   
--------------------------------------------------------------------------------
renderBoard     :: Board -> IO()
renderBoard b   =  mapM_ renderArea validPositions
  where renderArea p    = renderGameObject backBoard p
                          >> renderGameObject (disk $ b `refer` p) p
  
renderGameObject            :: GameObject -> Position -> IO()
renderGameObject obj (x, y) =  GLUT.preservingMatrix $
        GLUT.translate (GLUT.Vector3 x' y' 0 :: GLUT.Vector3 Double) >> obj
  where
    x' = (fromIntegral x - fromIntegral sizeX / 2) * unitAreaX * 2 + unitAreaX
    y' = (fromIntegral sizeY / 2 - fromIntegral y) * unitAreaY * 2 - unitAreaY
--------------------------------------------------------------------------------
--  ゲームオブジェクト
--------------------------------------------------------------------------------
type GameObject = IO()

backBoard   :: GameObject
backBoard   =  do
    GLUT.color (GLUT.Color3 0.2 0.5 0.2 :: GLUT.Color3 Double)
    GLUT.renderPrimitive GLUT.Quads $ do
        GLUT.vertex $ GLUT.Vertex3 (unitAreaX  * size) (unitAreaY  * size) 0
        GLUT.vertex $ GLUT.Vertex3 (-unitAreaX * size) (unitAreaY  * size) 0
        GLUT.vertex $ GLUT.Vertex3 (-unitAreaX * size) (-unitAreaY * size) 0
        GLUT.vertex $ GLUT.Vertex3 (unitAreaX  * size) (-unitAreaY * size) 0
  where size = 0.95

disk            :: Area -> GameObject
disk Space      =  return ()
disk c          =  do
    GLUT.color $ color' c
    GLUT.renderPrimitive GLUT.Polygon $ mapM_ GLUT.vertex
     $ shapeCircle (unitAreaX * size c) (unitAreaY * size c) 64
  where
    color' Black    = GLUT.Color3 0 0 0 :: GLUT.Color3 Double
    color' _        = GLUT.Color3 1 1 1 :: GLUT.Color3 Double
    size Black      = 0.93
    size _          = 0.90

-- 円形を作成。 X軸の長さ-> y軸の長さ-> 頂点の数->
shapeCircle :: Double -> Double -> Double -> [GLUT.Vertex3 GLUT.GLdouble]
shapeCircle rx ry n = [GLUT.Vertex3 (f cos rx v) (f sin ry v) 0 | v<-[0..(n-1)]]
  where  f f' r' v'  = r' * (f' $ v' / n * 2 * pi)