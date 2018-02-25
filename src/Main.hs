
module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

maxWidth, maxHight :: Float
maxWidth = 640
maxHight = 480

type Block = (Point, -- center
              Float, -- width
              Float, -- hight
              Color)

type Pad = (Float, --position
            Float, --speed
            Float, --size
            Color)

type Ball = (Point,  --center
             Vector, --speed
             Float,  --size
             Color)


data Shape = Block Block
           | Pad Pad
           | Ball Ball
           deriving(Show)


window :: Display
window = InWindow "Breakout" (round maxWidth,round maxHight) (0,0)

fps :: Int
fps = 60

-- Blocks

blockAreaSize :: Float
blockAreaSize = maxHight/2

blockLines, blockColumns :: Float
blockLines = 10
blockColumns = 10

genBlock :: Point ->  Block
genBlock (x,y) = ((x,y), maxWidth/blockColumns-1, blockAreaSize/blockLines-1, red)

genBlocksLine :: Float -> [Block]
genBlocksLine n = map f [-(blockColumns-1)/2..(blockColumns-1)/2] where
    f x = genBlock ((x)*maxWidth/blockColumns,
                    (n-0.5)*blockAreaSize/blockLines)

genBlocksGrid :: [Block]
genBlocksGrid = concat(map genBlocksLine [1..blockLines])

drawBlock :: Block -> Picture
drawBlock ((x,y),w,h,c) = color c (translate x y (rectangleSolid w h))

-- Pad

padSize :: Float
padSize = maxWidth/10

padSpeed :: Float
padSpeed = 100

genPad :: Pad
genPad = (0, 0, padSize, white)

drawPad :: Pad -> Picture
drawPad (p, v, s, c) = color c (translate p (-maxHight/2+1) (line [(-s/2, 0),(s/2,0)]))

updatePad :: Float -> Shape -> Shape
updatePad dt (Pad (p, v, s, c)) = Pad ((max (min (maxWidth/2) (p+v*dt)) (-maxWidth/2)), v, s, c)

-- Ball

ballSize :: Float
ballSize = padSize/10

ballPosition :: Point
ballPosition = (0, (-maxHight)/4)

ballSpeed :: Vector
ballSpeed = (50, -50)

genBall :: Ball
genBall = (ballPosition, ballSpeed, ballSize, white)

drawBall :: Ball -> Picture
drawBall ((x,y),v,s,c) = color c (translate x y (circleSolid s))


updateBall :: Float -> Shape -> Shape
updateBall dt (Ball ((x,y), (dx, dy), s, c)) = Ball ((x+dt*dx,y+dt*dy), (dx, dy), s, c)


--Shapes

drawShape :: Shape -> Picture
drawShape (Block b) = drawBlock b
drawShape (Pad p) = drawPad p
drawShape (Ball b) = drawBall b

genAllShapes :: [Shape]
genAllShapes = Pad genPad : Ball genBall : map Block genBlocksGrid

isBall :: Shape -> Bool
isBall (Ball _) = True
isBall _ = False

isPad :: Shape -> Bool
isPad (Pad _) = True
isPad _ = False

isBlock :: Shape -> Bool
isBlock (Block _) = True
isBlock _ = False

colisionsBlockBall :: Float -> Shape -> Shape -> Bool
colisionsBlockBall dt (Block ((x2,y2),w,h,_)) (Ball ((x1,y1),(dx,dy),s,_))
    | abs(x1+dt*dx-x2) <= w/2+s && abs(y1+dt*dy-y2) <= h/2+s = True
    | otherwise = False

colisionsBallWall :: Float -> Shape -> Shape
colisionsBallWall dt (Ball ((x,y),(dx,dy),s,c))
    | x+dt*dx+s > maxWidth/2 || x+dt*dx-s < -maxWidth/2 = Ball ((x,y),(-dx,dy),s,c)
    | y+dt*dy+s > maxHight/2 = Ball ((x,y),(dx,-dy),s,c)
    | otherwise = Ball ((x,y),(dx,dy),s,c)

colisionsBallBlock :: Float -> Shape -> Shape -> Shape
colisionsBallBlock dt (Ball ((x1,y1), (dx,dy), s, c)) (Block ((x2,y2), w, h, _))
    | abs(x1+dt*dx-x2) > w/2+s || abs(y1+dt*dy-y2) > h/2+s = Ball ((x1,y1),(dx,dy),s,c)
    | abs(x1-x2) > w/2+s && abs(x1+dt*dx-x2) <= w/2+s      = Ball ((x1,y1),(-dx,dy),s,c)
    | abs(y1-y2) > h/2+s && abs(y1+dt*dy-y2) <= h/2+s      = Ball ((x1,y1),(dx,-dy),s,c)
    | otherwise                                            = Ball ((x1,y1),(dx,dy),s,c)

colisionsBallVoid :: Float -> Shape -> Shape -> Bool
colisionsBallVoid dt (Ball ((x,y),(dx,dy),s1,_)) (Pad (p,v,s2,_))
    | y+dt*dy <= s1-maxHight/2 && abs(x+dt*dx-(p+dt*v)) >= s2 = True
    | otherwise                                               = False

padBounce :: Float -> Vector -> Vector
padBounce pos (dx,dy) = (v*sin(pos*pi/2), v*cos(pos*pi/2)) where
    v = sqrt(dx**2+dy**2)

colisionsBallPad :: Float -> Shape -> Shape -> Shape
colisionsBallPad dt (Ball ((x,y),(dx,dy),s1,c)) (Pad (p, v, s2, _))
    | y+dt*dy > s1-maxHight/2        = Ball ((x,y),(dx,dy),s1,c)
    | abs(x+dt*dx-p-dt*v) < s2 = Ball ((x,y),(padBounce ((x+dt*dx-p-dt*v)/s2) (dx,dy)),s1,c)


update :: Float -> [Shape] -> [Shape]
update dt shapes = pads ++ balls ++ blocks where
    allPads = filter isPad shapes
    allBalls = filter isBall shapes
    allBlocks = filter isBlock shapes
    balls1 = [ colisionsBallWall dt ball 
             | ball<-allBalls]
    balls2 = [ colisionsBallPad dt ball pad 
             | ball<-balls1, pad<-allPads, not (colisionsBallVoid dt ball pad)]
    balls3 = [ foldl (colisionsBallBlock dt) ball allBlocks 
             | ball<-balls2]
    balls = [ updateBall dt ball 
            | ball<-balls3]
    blocks = [ block 
             | block<-allBlocks, not (any (colisionsBlockBall dt block) allBalls)]
    pads = [ updatePad dt pad 
           | pad<-allPads] 


render :: [Shape] -> Picture
render shapes = pictures (map drawShape shapes)



react :: Event -> [Shape] -> [Shape]
react event [] = []
react (EventKey (SpecialKey KeyLeft) Down _ _) ((Pad (p, v, s, c)):xs) = (Pad (p, -padSpeed, s, c)):xs
react (EventKey (SpecialKey KeyRight) Down _ _) ((Pad (p, v, s, c)):xs) = (Pad (p, padSpeed, s, c)):xs
react _ ((Pad (p, v, s, c)):xs) = (Pad (p, 0, s, c)):xs
react event (x:xs) = x : (react event xs)



main :: IO ()
main = do 
    play window black fps genAllShapes render react update


