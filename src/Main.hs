module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random

maxWidth, maxHight :: Float
maxWidth = 640
maxHight = 480

window :: Display
window = InWindow "Breakout" (round maxWidth,round maxHight) (0,0)

fps :: Int
fps = 60

-- currently all blocks have the seme size but that can easily be changed
-- by changing the level generation process
type Block = (Point, -- center
              Float, -- width
              Float, -- hight
              Int)   -- type/color

type Pad = (Float, --position
            Float, --speed
            Float, --size
            Color)

type Ball = (Point,  --center
             Vector, --speed
             Float,  --size
             Int)    --bonus level


data GameState = GameState
        [Pad]  -- List of pads is being used as a generalisation for possible features to be implemented
        [Ball]
        [Block]
        StdGen
        deriving(Show)


colors :: [Color]
colors = [yellow, red, green, blue]

-- Blocks

blockAreaSize :: Float
blockAreaSize = maxHight/2

blockLines, blockColumns :: Float
blockLines = 10
blockColumns = 10

genBlock :: Point ->  Block
genBlock (x,y) = ((x,y), maxWidth/blockColumns-1, blockAreaSize/blockLines-1, 0)

genBlocksLine :: Float -> [Block]
genBlocksLine n = map f [-(blockColumns-1)/2..(blockColumns-1)/2] where
    f x = genBlock ((x)*maxWidth/blockColumns,
                    (n-0.5)*blockAreaSize/blockLines)

genBlocksGrid :: [Block]
genBlocksGrid = concat(map genBlocksLine [1..blockLines])

randomizeBlocks :: (RandomGen g) => [Block] -> g -> ([Block], g)
randomizeBlocks [] g = ([], g)
randomizeBlocks ((p,w,h,_):xs) g = (((p,w,h,t):rest), gf) where
    (t,gn) = randomR (0,(length colors)-1) g 
    (rest, gf) = randomizeBlocks xs gn

drawBlock :: Block -> Picture
drawBlock ((x,y),w,h,c) = color (colors!!c) (translate x y (rectangleSolid w h))

-- Pad

padSize :: Float
padSize = maxWidth/10

padSpeed :: Float
padSpeed = 200

genPad :: Pad
genPad = (0, 0, padSize, white)

drawPad :: Pad -> Picture
drawPad (p, v, s, c) = color c (translate p (-maxHight/2+1) (line [(-s/2, 0),(s/2,0)]))

updatePad :: Float -> Pad -> Pad
updatePad dt (p, v, s, c) = ((max (min (maxWidth/2) (p+v*dt)) (-maxWidth/2)), v, s, c)

-- Ball

ballSize :: Float
ballSize = padSize/10

ballPosition :: Point
ballPosition = (0, (-maxHight)/8)

ballSpeed :: Vector
ballSpeed = (100, -100)

genBall :: Ball
genBall = (ballPosition, ballSpeed, ballSize, 0)

drawBall :: Ball -> Picture
drawBall ((x,y),v,s,c) = color (colors!!c) (translate x y (circleSolid s))

updateBall :: Float -> Ball -> Ball
updateBall dt ((x,y), (dx, dy), s, c) = ((x+dt*dx,y+dt*dy), (dx, dy), s, c)

duplicateBalls :: [Ball] -> [Ball]
duplicateBalls [] = []
duplicateBalls ((p,(dx,dy),s,b):xs) = if b >= 4 
    then (p,(-dx,dy),ballSize,0):(p,(dx,-dy),ballSize,0):(duplicateBalls xs)
    else (p,(dx,dy),s,b):(duplicateBalls xs)

-- GameState

genGameState :: StdGen -> GameState
genGameState g = GameState [genPad] [genBall] blocks gn where
    (blocks, gn) = randomizeBlocks genBlocksGrid g

-- Colisions

colisionsBlockBall :: Float -> Block -> Ball -> Bool
colisionsBlockBall dt ((x2,y2),w,h,_) ((x1,y1),(dx,dy),s,_)
    | abs(x1+dt*dx-x2) <= w/2+s && abs(y1+dt*dy-y2) <= h/2+s = True
    | otherwise = False

colisionsBallWall :: Float -> Ball -> Ball
colisionsBallWall dt ((x,y),(dx,dy),s,c)
    | x+dt*dx+s > maxWidth/2 || x+dt*dx-s < -maxWidth/2 = ((x,y),(-dx,dy),s,c)
    | y+dt*dy+s > maxHight/2                            = ((x,y),(dx,-dy),s,c)
    | otherwise                                         = ((x,y),(dx,dy),s,c)


bonus :: Int -> Ball -> Ball
bonus b (p,(dx,dy),s,c)
    | b==0 = (p,(dx,dy),s,c)
    | b==1 = (p,(dx*1.2,dy*1.2),s,c)
    | b==2 = (p,(dx/1.2,dy/1.2),s,c)
    | b==3 = (p,(dx,dy),s*1.2,c+1)

colisionsBallBlock :: Float -> Ball -> Block -> Ball
colisionsBallBlock dt ((x1,y1), (dx,dy), s, c) ((x2,y2), w, h, t) = (bonus x ((x1,y1),(dx',dy'), s, c)) where
    dx' = colision x1 x2 dx w y1 y2 dy h
    dy' = colision y1 y2 dy h x1 x2 dx w
    colision i1 i2 di i j1 j2 dj j = 
        if abs(i1-i2) > i/2+s && abs(i1+dt*di-i2) <= i/2+s && abs(j1+dt*dj-j2) <= j/2+s
        then -di
        else di
    x = if dx == dx' && dy == dy'
    then 0
    else t

colisionsBallAllBlocks :: Float -> [Block] -> Ball -> Ball
colisionsBallAllBlocks dt blocks ball = foldl (colisionsBallBlock dt) ball blocks

colisionsBallVoid :: Float -> Ball -> Pad -> Bool
colisionsBallVoid dt ((x,y),(dx,dy),s1,_) (p,v,s2,_)
    | y+dt*dy <= s1-maxHight/2 && abs(x+dt*dx-(p+dt*v)) >= s2 = True
    | otherwise                                               = False

padBounce :: Float -> Vector -> Vector
padBounce pos (dx,dy) = (v*sin(pos*pi/2), v*cos(pos*pi/2)) where
    v = sqrt(dx**2+dy**2)

colisionsBallPad :: Float -> Ball -> Pad -> Ball
colisionsBallPad dt ((x,y),(dx,dy),s1,c) (p, v, s2, _)
    | y+dt*dy > s1-maxHight/2  = ((x,y),(dx,dy),s1,c)
    | abs(x+dt*dx-p-dt*v) < s2 = ((x,y),(padBounce ((x+dt*dx-p-dt*v)/s2) (dx,dy)),s1,c)

colisionsBallAllPads :: Float -> [Pad] -> Ball -> Ball
colisionsBallAllPads dt pads ball = foldl (colisionsBallPad dt) ball pads


update :: Float -> GameState -> GameState
-- new level
update _ (GameState allPads allBalls [] g) = GameState allPads allBalls newBlocks gn where
    (newBlocks, gn) = randomizeBlocks genBlocksGrid g
-- time update
update dt (GameState allPads allBalls allBlocks g) = GameState pads balls blocks g where
    -- update balls for colisions
    balls1 = [ (updateBall dt . colisionsBallAllBlocks dt allBlocks . colisionsBallAllPads dt allPads . colisionsBallWall dt) ball
             | ball<-allBalls, any (not . colisionsBallVoid dt ball) allPads]
    -- duplicate balls when they reach max bonus
    balls = duplicateBalls balls1
    -- remove blocks that were hit
    blocks = [ block 
             | block<-allBlocks, not (any (colisionsBlockBall dt block) allBalls)]
    -- move pads
    pads = [ updatePad dt pad 
           | pad<-allPads] 

-- draw everything
render :: GameState -> Picture
render (GameState pads balls blocks _) = pictures ((map drawPad pads) ++ (map drawBall balls) ++ (map drawBlock blocks))

-- control the first pad with arrow keys
react :: Event -> GameState -> GameState
react event (GameState [] balls blocks g) = GameState [] balls blocks g
react (EventKey (SpecialKey KeyLeft) Down _ _) (GameState ((p, v, s, c):xs) balls blocks g) =
    GameState ((p, v-padSpeed, s, c):xs) balls blocks g
react (EventKey (SpecialKey KeyRight) Down _ _) (GameState ((p, v, s, c):xs) balls blocks g) =
    GameState ((p, v+padSpeed, s, c):xs) balls blocks g
react (EventKey (SpecialKey KeyLeft) Up _ _) (GameState ((p, v, s, c):xs) balls blocks g) =
    GameState ((p, v+padSpeed, s, c):xs) balls blocks g
react (EventKey (SpecialKey KeyRight) Up _ _) (GameState ((p, v, s, c):xs) balls blocks g) =
    GameState ((p, v-padSpeed, s, c):xs) balls blocks g
react _ x = x


main :: IO ()
main = do
    play window black fps (genGameState (mkStdGen 0)) render react update
