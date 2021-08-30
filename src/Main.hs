module Main where

{-# LANGUAGE FlexibleContexts #-}  

import Graphics.Gloss.Interface.Pure.Game
import System.Random

type Velocity = (Float, Float)
type Position = (Float, Float)
type Angle = Float

type Cannon = (Position, Velocity)

type Laser = (Position, Velocity, Picture)

type Demon = (Position, Velocity, Angle)

type World = (Cannon, [Laser], [Demon], StdGen, Bool)

drawWorld :: World -> Picture 
drawWorld (_,_,_,_, True) = beginingScreen
drawWorld (c,ls,ds,g,_) = color white $ pictures w
   where
     w = drawCannon c : lasersAndDemons
     lasersAndDemons = lasers ++ demons
     lasers = map drawLaser ls
     demons = map drawDemons ds

drawLaser :: Laser -> Picture 
drawLaser ((x, y), (_,_), p) = translate x y p

drawDemons :: Demon -> Picture
drawDemons ((x, y), (_,_), _) = translate x y p
  where
    p = color green $ pictures [circleSolid 10.0, rectangleSolid 35 2] 

drawCannon :: Cannon -> Picture 
drawCannon ((x, y), (_,_)) = translate x y p
   where
     p = color white $ pictures ([circleSolid 10.0, rectangleSolid 2 30])

beginingScreen :: Picture 
beginingScreen = color white $ pictures screen
   where
     screen = [start] ++ [name] ++ instructions
     start = (translate (-165.0) (-80.0) . scale 0.2 0.2) $ text "Press the mouse left button"
     name = (translate (-165.0) (0.0) . scale 0.4 0.4) $ text "Demon Invasion"
     instructions = [commandLeft]++[commandRight]++[commandSpace]
     commandLeft = (translate (-380.0) (-200.0) . scale 0.1 0.1) $ text "Press <- to move left" 
     commandRight = (translate (-380.0) (-220.0) . scale 0.1 0.1) $ text "Press -> to move right"
     commandSpace = (translate (-380.0) (-240.0) . scale 0.1 0.1) $ text "Press space to shot"

initialWorld :: StdGen -> World 
initialWorld g = (c, ls, ds, g, False)
   where
     c = ((0.0, -240.0), (0.0, 0.0))
     ls = []
     ds = demonsCreating 15 g -- demons


demonsCreating :: Int -> StdGen -> [Demon]
demonsCreating n g = take n $ zip3 coords vels angs
   where
     (posGen, angGen) = split g
     pGen = fst $ split posGen
     (xGen, yGen) = split pGen
     vels = cycle[(-80,-80), (80,90), (80,-70), (10,170), (-100,110), (150, 5)]
     angs = map (2*pi*) (randoms angGen)    
     convCoord x y = (x * 2 * limiteX - limiteX, y * 2 * limiteY - limiteY)
     coords = zipWith convCoord (randoms xGen) (randoms yGen)
     
updateWorld :: Float -> World -> World
updateWorld  dt world@(c, ls, ds, g, boolean) 
    | boolean = world   
    | cannonShoted c ds = initialWorld (fst$ split g)
    | otherwise = (c_, ls_, ds_, (fst $ split g), boolean)
          where
            c_  = (updatePosition dt . updateVelocity dt) c
            ls_ = map (updateLaserPosition dt) lasersNotShoted
            ds_ | null demonsAlive = demonsCreating 15 g
                | otherwise = map (updateDemonPosition dt. updateDemonVelocity dt ) demonsAlive            
            (demonsAlive,lasersNotShoted) = demonsKilled  (ds, ls)


demonsKilled ::  ([Demon],[Laser]) -> ([Demon], [Laser])
demonsKilled  (ds,ls) = (ds_,ls_)
   where
     (dsAux, lsAux) = verifyShocks (ds,ls)   
     ls_= map (fst) (filter snd lsAux) 
     ds_= (map (fst) (filter snd dsAux))

verifyShocks :: ([Demon],[Laser]) -> ([(Demon,Bool)], [(Laser,Bool)])
verifyShocks (ds,ls) = ((verifyDemon ls ds), (verifyLaser ds ls))

verifyDemon :: [Laser] -> [Demon] -> [(Demon,Bool)]
verifyDemon ls ds = zip ds (map (verifyDemonPoint ls) ds) 

verifyLaser :: [Demon] -> [Laser] -> [(Laser,Bool)]
verifyLaser ds ls = zip ls (map (verifyLaserPoint ds) ls)

verifyLaserPoint :: [Demon] -> Laser -> Bool
verifyLaserPoint [] _ = True
verifyLaserPoint (((x1, y1), (vx1,vy1), ang) : ds) laser@((x, y), (vx, vy), _)
    | distance (x1,y1) (x,y) <= 10.0 = False 
    | otherwise = verifyLaserPoint ds laser      
  
verifyDemonPoint :: [Laser] -> Demon -> Bool
verifyDemonPoint [] _ = True
verifyDemonPoint (((x1, y1), (vx1,vy1), p) : ls) demon@((x, y), (vx, vy), ang)
    |  distance (x1,y1) (x,y) <= 10.0 = False 
    | otherwise = verifyDemonPoint ls demon    

cannonShoted :: Cannon -> [Demon] -> Bool
cannonShoted c ds =  any (cannonDestroyed c) ds

cannonDestroyed :: Cannon -> Demon -> Bool
cannonDestroyed (coordC@(xc, yc), (_,_)) ((coordD@(xd, yd), (_,_), _))   
   | distance coordC coordD < 10 = True
   | otherwise = False    
    
distance :: (Float,Float) -> (Float,Float) -> Float 
distance (x1,y1) (x2,y2) = sqrt $ (x1-x2)**2 + (y1-y2)**2

updatePosition :: Float -> Cannon -> Cannon
updatePosition dt ((x,y), (vx, vy)) = (((x + vx * dt), y), (vx, vy))

updateVelocity :: Float -> Cannon -> Cannon
updateVelocity dt ((x,y), (vx, vy)) 
   | x < -lx = ((-lx, y), (-vx, vy))
   | x >  lx = ((lx, y), (-vx, vy))
   | otherwise = ((x,y), (vx, vy))
    where
      lx = limiteX - 10

updateLaserPosition :: Float -> Laser -> Laser
updateLaserPosition dt (((x,y), (vx, vy), p)) = 
     ((x, y + vy*dt), (vx, vy), p)


updateDemonPosition :: Float -> Demon -> Demon 
updateDemonPosition dt ((x, y), (vx, vy), ang) = 
     ((x + vx*dt, y + vy*dt), (vx, vy), ang) 

updateDemonVelocity :: Float -> Demon -> Demon 
updateDemonVelocity dt ((x, y), (vx, vy), ang)
   | x < -lx = ((-lx, y), (-vx, vy), ang)
   | x >  lx = ((lx, y), (-vx, vy), ang)
   | y >  ly = ((x, ly), (vx, -vy), ang)
   | y < -ly = ((x, -ly), (vx, -vy), ang)
   | otherwise  = ((x, y), (vx, vy), ang) 
   where
     lx = limiteX - 10
     ly = limiteY - 10 

moveTheCannon :: Cannon -> Float -> Cannon
moveTheCannon ((x, y), (vx, vy)) pos = ((x + pos, y), (vx, vy))

handleKeys :: Event -> World -> World
handleKeys (EventKey (MouseButton LeftButton ) Down  _ _ ) (c, ls, ds, g, True) = initialWorld g
handleKeys (EventKey (SpecialKey KeySpace) Up  _ _ ) (c, ls, ds, g, False ) = (c, ls, ds, g, False)
handleKeys (EventKey (SpecialKey KeySpace) Down  _ _ ) (c, ls, ds, g, False ) = (c, laserShot c ls (0,500), ds, g, False) 
handleKeys (EventKey (SpecialKey KeyLeft ) Up  _ _ ) (c, ls, ds, g, False ) = (moveTheCannon c 0,ls, ds, g, False)
handleKeys (EventKey (SpecialKey KeyLeft ) Down  _ _ ) (c, ls, ds, g, False ) = (moveTheCannon c (-30),ls, ds, g, False) 
handleKeys (EventKey (SpecialKey KeyRight) Up  _ _ ) (c, ls, ds, g, False ) = (moveTheCannon c 0,ls, ds, g, False)
handleKeys (EventKey (SpecialKey KeyRight) Down  _ _ ) (c, ls, ds, g, False ) = (moveTheCannon c 30,ls, ds, g, False)
handleKeys _ world = world

-- the laser shot just move in y-axis
laserShot :: Cannon -> [Laser] -> Velocity -> [Laser] 
laserShot ((x,y), _) ls (vx, vy) = ((x, y + 15*cos 0), (vx, vy), shot) : ls 
   where
     shot = color white $ circleSolid 1

pointZero :: StdGen -> World
pointZero g = (((0,0), (0,0)), [], [], g, True)

limiteX :: Num a => a
limiteX = 400

limiteY :: Num a => a
limiteY = 300

main :: IO ()
main = do

  gen <- getStdGen 

  let world = pointZero gen

  play
    window
    black 
    60
    world
    drawWorld
    handleKeys
    updateWorld
    where
      window = InWindow "Demon Invasion" ((2*limiteX, 2*limiteY)) ((80, 60))
