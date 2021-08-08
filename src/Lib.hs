module Lib
    ( runGame
    ) where

import Tools

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Game

import qualified Data.List.Zipper as Z

-- | ((x, y), (vx, vy), pic)
data Alien = 
    Alien ((Float, Float), (Float, Float), Picture) | Destroyed
    deriving (Eq)

data Bullet =
    Bullet ((Float, Float), (Float, Float), Picture) | Spent
    deriving (Eq)

type Player = (Float, Picture)

class Drawable a where
    draw :: a -> Picture

instance Drawable Alien where
    draw (Alien x) = drawAlien (Alien x)
    draw Destroyed = blank

instance Drawable Bullet where
    draw (Bullet x) = drawBullet (Bullet x)
    draw Spent = blank

data WorldState =
    WorldState {
        aliens      :: Z.Zipper [Alien],
        alienLine   :: Int,
        player      :: Player,
        bullets     :: [Bullet],
        speed       :: Float
    }

width, height, alienSize, bulletSize :: Num a => a
width = 200
height = 200
alienSize = 10
bulletSize = 5

-- | Jogador
getPlayerPosition :: Player -> Float
getPlayerPosition (x, p) = x

drawPlayer :: Player -> Picture
drawPlayer (x, p) = translate x (-280) p

updatePlayerPosition :: Float -> Player -> Player
updatePlayerPosition direction (x, p) =
    (x + 10 * direction, p)

-- | Projéteis
createBullet :: Float -> Float -> Float -> Color -> Float -> Bullet
createBullet x y vy c size = Bullet ((x, y), (0, vy), color c $ rectangleSolid size size)

drawBullet :: Bullet -> Picture
drawBullet (Bullet ((x, y), _, p)) = translate x y p

updateBulletPosition :: Float -> Float -> Bullet -> Bullet
updateBulletPosition _ _ Spent = Spent
updateBulletPosition speed dt (Bullet ((x, y), (vx, vy), p))
    | y < adjY  = Bullet ((x, y + vy * speed * dt), (vx, vy), p)
    | otherwise = Spent
    where 
        adjY = 1.5 * height - alienSize / 2

updateBullet :: Float -> Float -> [Bullet] -> [Bullet]
updateBullet speed dt bullets = filter (/= Spent) (map (updateBulletPosition speed dt) bullets)

-- | Alien
createAlien :: Float -> Float -> Float -> Color -> Float -> Alien
createAlien x y vx c size = Alien ((x, y), (vx, 0), color c $ rectangleSolid size size)

drawAlien :: Alien -> Picture
drawAlien (Alien ((x, y), _, p)) = translate x y p

applyChanges :: Z.Zipper [Alien] -> Int -> Float -> Float -> Z.Zipper [Alien]
applyChanges alien n speed dt = Z.replace (fmap (updateAlienVelocity . updateAlienPosition speed dt) (Z.cursor (goRightNTimes alien n))) (goRightNTimes alien n)

updateAlien :: Int -> Float -> Float -> Z.Zipper [Alien] -> Z.Zipper [Alien]
updateAlien n speed dt aliens = applyChanges aliens n speed dt

updateAlienPosition :: Float -> Float -> Alien -> Alien
updateAlienPosition _ _ Destroyed = Destroyed
updateAlienPosition speed dt (Alien ((x, y), v@(vx, vy), p)) = Alien ((x + vx * speed * dt, y), v, p)

updateAlienVelocity :: Alien -> Alien
updateAlienVelocity Destroyed = Destroyed
updateAlienVelocity (Alien b@((x, y), (vx, vy), p))
        | x < -adjX       = Alien ((-adjX, y - 20), (-vx, vy), p)
        | x > adjX        = Alien ((adjX, y - 20), (-vx, vy), p)
        | y < -adjY + 40  = Alien ((x, -adjY), (vx, vy), blank)
        | y > adjY        = Alien ((x, adjY), (vx, vy), p)
        | otherwise       = Alien ((x, y), (vx, vy), p)
    where
        adjX = width - alienSize / 2
        adjY = 1.5 * height - alienSize / 2

-- | Desenho dos modelos
drawModels :: WorldState -> Picture
drawModels ws = pictures $ fmap draw (concat $ Z.toList $ aliens ws) ++ [drawPlayer $ player ws] ++ map draw (bullets ws)

-- | Gerenciador de eventos com interação do usuário
eventHandler :: Event -> WorldState -> WorldState
eventHandler (EventKey (SpecialKey x) Down _ _) ws = case x of
    KeyLeft -> WorldState (aliens ws) (alienLine ws) player' (bullets ws) (speed ws)
    KeyRight -> WorldState (aliens ws) (alienLine ws) player'' (bullets ws) (speed ws)
    KeySpace -> WorldState (aliens ws) (alienLine ws) (player ws) bullets' (speed ws)
    _ -> ws
    where
        player'     = updatePlayerPosition (-1) (player ws)
        player''    = updatePlayerPosition 1 (player ws)
        bullets'    = 
            if length (bullets ws) < 3 
                then createBullet (getPlayerPosition $ player ws) (-270) 50 black bulletSize : bullets ws 
            else bullets ws
eventHandler _ ws = ws

-- | Atualizador de modelo a cada frame
updateModel :: Float -> WorldState -> WorldState
updateModel dt ws = WorldState aliens' alienLine' player' bullets' speed'
    where
        aliens' = goLeftNTimes (updateAlien (alienLine ws) (speed ws) dt (aliens ws)) 3
        alienLine' = (alienLine ws + 1) `mod` 4
        player' = player ws
        speed' = speed ws
        bullets' = updateBullet (speed ws) dt (bullets ws)

runGame :: IO ()
runGame = do
    let
        aliensLine1 = [createAlien x y 25 red alienSize | x <- [(-80), (-60) .. 80], y <- [40]]
        aliensLine2 = [createAlien x y 25 blue alienSize | x <- [(-80), (-60) .. 80], y <- [80]]
        aliensLine3 = [createAlien x y 25 green alienSize | x <- [(-80), (-60) .. 80], y <- [120]]
        aliensLine4 = [createAlien x y 25 yellow alienSize | x <- [(-80), (-60) .. 80], y <- [160]]
        aliens = Z.fromList [aliensLine1, aliensLine2, aliensLine3, aliensLine4]
        player = (0, color blue $ rectangleSolid alienSize alienSize)
        bullets = []
        initialWorld = WorldState aliens 0 player bullets 1
    play
        (InWindow "SpaceInvadersClone" (width * 2, height * 3) (100, 100))
        white
        15
        initialWorld
        drawModels
        eventHandler
        updateModel
