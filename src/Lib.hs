module Lib
    ( runGame
    ) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Game

import qualified Data.List.Zipper as Z

-- | ((x, y), (vx, vy), pic)
type Alien = ((Float, Float), (Float, Float), Picture)

type Player = (Float, Picture)

data WorldState =
    WorldState {
        aliens      :: Z.Zipper Alien,
        alienLine   :: Int,
        player      :: Player,
        speed       :: Float
    }

width, height, alienSize :: Num a => a
width = 200
height = 200
alienSize = 10

-- | Jogador
drawPlayer :: Player -> Picture
drawPlayer (x, p) = translate x (-280) p

updatePlayerPosition :: Float -> Player -> Player
updatePlayerPosition direction (x, p) =
    (x + 10 * direction, p)

-- | Alien
drawAlien :: Alien -> Picture
drawAlien ((x, y), (vx, vy), p) = translate x y p

goRightNTimes :: Z.Zipper a -> Int -> Z.Zipper a
goRightNTimes z 0 = z
goRightNTimes z n
  | Z.endp z  = z
  | otherwise = goRightNTimes (Z.right z) (n - 1)

updateAlien :: Int -> Float -> Float -> Z.Zipper Alien -> Z.Zipper Alien
updateAlien n speed dt = fmap (updateAlienVelocity . updateAlienPosition speed dt)

updateAlienPosition :: Float -> Float -> Alien -> Alien
updateAlienPosition speed dt ((x, y), v@(vx, vy), p) = ((x + vx * speed * dt, y + vy * dt), v, p)

updateAlienVelocity :: Alien -> Alien
updateAlienVelocity b@((x, y), (vx, vy), p)
        | x < -adjX  = ((-adjX, y - 20), (-vx, vy), p)
        | x > adjX   = ((adjX, y - 20), (-vx, vy), p)
        | y < -adjY  = ((x, -adjY), (vx, vy), p)
        | y > adjY   = ((x, adjY), (vx, vy), p)
        | otherwise  = ((x, y), (vx, vy), p)
    where
        adjX = width - alienSize / 2
        adjY = 1.5 * height - alienSize / 2

-- | Desenho dos modelos
drawModels :: WorldState -> Picture
drawModels ws = pictures (map drawAlien (Z.toList $ aliens ws) ++ [drawPlayer (player ws)])

-- | Gerenciador de eventos com interação do usuário
eventHandler :: Event -> WorldState -> WorldState
eventHandler (EventKey (SpecialKey x) Down _ _) ws = case x of
    KeyLeft -> WorldState (aliens ws) (alienLine ws) player' (speed ws)
    KeyRight -> WorldState (aliens ws) (alienLine ws) player'' (speed ws)
    KeySpace -> WorldState (aliens ws) (alienLine ws) (player ws) (speed ws + 0.1)
    _ -> ws
    where
        player' = updatePlayerPosition (-1) (player ws)
        player'' = updatePlayerPosition 1 (player ws)
eventHandler _ ws = ws

-- | Atualizador de modelo a cada frame
updateModel :: Float -> WorldState -> WorldState
updateModel dt ws = WorldState aliens' alienLine' player' speed'
    where
        aliens' = updateAlien (alienLine ws) (speed ws) dt (aliens ws)
        alienLine'
            | alienLine ws <= 4 = alienLine ws + 1
            | otherwise         = 1
        player' = player ws
        speed' = speed ws

runGame :: IO ()
runGame = do
    let
        aliensLine1 = [((x, y), (25, 0), color red $ rectangleSolid alienSize alienSize) | x <- [(-80), (-60) .. 80], y <- [40]]
        aliensLine2 = [((x, y), (25, 0), color blue $ rectangleSolid alienSize alienSize) | x <- [(-80), (-60) .. 80], y <- [80]]
        aliensLine3 = [((x, y), (25, 0), color green  $ rectangleSolid alienSize alienSize) | x <- [(-80), (-60) .. 80], y <- [120]]
        aliensLine4 = [((x, y), (25, 0), color yellow  $ rectangleSolid alienSize alienSize) | x <- [(-80), (-60) .. 80], y <- [160]]
        aliens = Z.fromList (aliensLine1 ++ aliensLine2 ++ aliensLine3 ++ aliensLine4)
        -- aliens = [((x, y), (25, 0), color red $ rectangleSolid alienSize alienSize) | x <- [(-80), (-60) .. 80], y <- [40, 80 .. 240]]
        player = (0, color blue $ rectangleSolid alienSize alienSize)
        initialWorld = WorldState aliens 0 player 1
    play
        (InWindow "SpaceInvadersClone" (width * 2, height * 3) (100, 100))
        white
        15
        initialWorld
        drawModels
        eventHandler
        updateModel
