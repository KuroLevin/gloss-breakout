module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random

maxWidth, maxHight :: Float
maxWidth = 640
maxHight = 480

type Block = (Point, -- center
              Float, -- width
              Float, -- hight
              Int)

type Pad = (Float, --position
            Float, --speed
            Float, --size
            Color)

type Ball = (Point,  --center
             Vector, --speed
             Float,  --size
             Int)    --bonus


data Shapes = Shapes
        [Pad]
        [Ball]
        [Block]
        deriving(Show)


window :: Display
window = InWindow "Breakout" (round maxWidth,round maxHight) (0,0)

fps :: Int
fps = 60

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

randomizeBlocks :: (RandomGen g) => [Block] -> g -> [Block]
randomizeBlocks [] _ = []
randomizeBlocks ((p,w,h,_):xs) g = (p,w,h,t):(randomizeBlocks xs gn) where
    (t,gn) = randomR (0,(length colors)-1) g 

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
    then (p,(-dx,dy),s,0):(p,(dx,-dy),s,0):(duplicateBalls xs)
    else (p,(dx,dy),s,b):(duplicateBalls xs)

--Shapes

genAllShapes :: (RandomGen g) => g -> Shapes
genAllShapes g = Shapes [genPad] [genBall] (randomizeBlocks genBlocksGrid g)

colisionsBlockBall :: Float -> Block -> Ball -> Bool
colisionsBlockBall dt ((x2,y2),w,h,_) ((x1,y1),(dx,dy),s,_)
    | abs(x1+dt*dx-x2) <= w/2+s && abs(y1+dt*dy-y2) <= h/2+s = True
    | otherwise = False

colisionsBallWall :: Float -> Ball -> Ball
colisionsBallWall dt ((x,y),(dx,dy),s,c)
    | x+dt*dx+s > maxWidth/2 || x+dt*dx-s < -maxWidth/2 = ((x,y),(-dx,dy),s,c)
    | y+dt*dy+s > maxHight/2                            = ((x,y),(dx,-dy),s,c)
    | otherwise                                         = ((x,y),(dx,dy),s,c)

colisionsBallBlock :: Float -> Ball -> Block -> Ball
colisionsBallBlock dt ((x1,y1), (dx,dy), s, c) ((x2,y2), w, h, t)
    | abs(x1+dt*dx-x2) > w/2+s || abs(y1+dt*dy-y2) > h/2+s    = ((x1,y1),(dx,dy),s,c)
    | abs(x1-x2) > w/2+s && abs(x1+dt*dx-x2) <= w/2+s && t==1 = ((x1,y1),(-dx*1.2,dy*1.2),s,c)
    | abs(y1-y2) > h/2+s && abs(y1+dt*dy-y2) <= h/2+s && t==1 = ((x1,y1),(dx*1.2,-dy*1.2),s,c)
    | abs(x1-x2) > w/2+s && abs(x1+dt*dx-x2) <= w/2+s && t==2 = ((x1,y1),(-dx/1.2,dy/1.2),s,c)
    | abs(y1-y2) > h/2+s && abs(y1+dt*dy-y2) <= h/2+s && t==2 = ((x1,y1),(dx/1.2,-dy/1.2),s,c)
    | abs(x1-x2) > w/2+s && abs(x1+dt*dx-x2) <= w/2+s && t==3 = ((x1,y1),(-dx,dy),s,c+1)
    | abs(y1-y2) > h/2+s && abs(y1+dt*dy-y2) <= h/2+s && t==3 = ((x1,y1),(dx,-dy),s,c+1)
    | abs(x1-x2) > w/2+s && abs(x1+dt*dx-x2) <= w/2+s         = ((x1,y1),(-dx,dy),s,c)
    | abs(y1-y2) > h/2+s && abs(y1+dt*dy-y2) <= h/2+s         = ((x1,y1),(dx,-dy),s,c)
    | otherwise                                               = ((x1,y1),(dx,dy),s,c)

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


update :: Float -> Shapes -> Shapes
update dt (Shapes allPads allBalls allBlocks) = Shapes pads balls blocks where
    balls1 = [ updateBall dt (foldl 
                                (colisionsBallBlock dt) 
                                (foldl 
                                    (colisionsBallPad dt) 
                                    (colisionsBallWall dt ball) 
                                    allPads) 
                                allBlocks)
            | ball<-allBalls, any (not . colisionsBallVoid dt ball) allPads]
    balls = duplicateBalls balls1
    blocks = [ block 
             | block<-allBlocks, not (any (colisionsBlockBall dt block) allBalls)]
    pads = [ updatePad dt pad 
           | pad<-allPads] 


render :: Shapes -> Picture
render (Shapes pads balls blocks) = pictures ((map drawPad pads) ++ (map drawBall balls) ++ (map drawBlock blocks))


react :: Event -> Shapes -> Shapes
react event (Shapes [] balls blocks) = Shapes [] balls blocks
react (EventKey (SpecialKey KeyLeft) Down _ _) (Shapes ((p, v, s, c):xs) balls blocks) =
    Shapes ((p, v-padSpeed, s, c):xs) balls blocks
react (EventKey (SpecialKey KeyRight) Down _ _) (Shapes ((p, v, s, c):xs) balls blocks) =
    Shapes ((p, v+padSpeed, s, c):xs) balls blocks
react (EventKey (SpecialKey KeyLeft) Up _ _) (Shapes ((p, v, s, c):xs) balls blocks) =
    Shapes ((p, v+padSpeed, s, c):xs) balls blocks
react (EventKey (SpecialKey KeyRight) Up _ _) (Shapes ((p, v, s, c):xs) balls blocks) =
    Shapes ((p, v-padSpeed, s, c):xs) balls blocks
react _ x = x


main :: IO ()
main = do
    play window black fps (genAllShapes (mkStdGen 0)) render react update
