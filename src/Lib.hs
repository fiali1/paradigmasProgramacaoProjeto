module Lib
    ( runGame
    ) where

import Tools

import Graphics.Gloss

import Graphics.Gloss.Interface.IO.Game

import qualified Data.List.Zipper as Z

import System.Random

-- DEFINIÇÕES DE TIPOS E VALORES-BASE ----------------------------------
data Alien =
    Alien ((Float, Float), (Float, Float), Picture) | Destroyed | Landed
    deriving (Eq)

data Bullet =
    Bullet ((Float, Float), (Float, Float), Picture) | Spent
    deriving (Eq)

data Barrier = 
    Barrier ((Float, Float), Picture) | ShotDown
    deriving (Eq)

type Player = (Float, Picture)

class Drawable a where
    draw :: a -> Picture

instance Drawable Alien where
    draw (Alien x) = drawAlien (Alien x)
    draw Destroyed = blank
    draw Landed    = blank

instance Drawable Bullet where
    draw (Bullet x) = drawBullet (Bullet x)
    draw Spent = blank

instance Drawable Barrier where
    draw (Barrier x) = drawBarrier (Barrier x)
    draw ShotDown    = blank

data WorldState =
    WorldState {
        alienModel   :: [Picture],
        aliens       :: Z.Zipper [Alien],
        alienLine    :: Int,
        player       :: Player,
        barriers     :: Z.Zipper [Barrier],
        bullets      :: [Bullet],
        alienBullets :: ([Bullet], StdGen),
        speed        :: Float,
        score        :: Int,
        gameOver     :: Bool,
        winFlag      :: Bool,
        gameRound    :: Int
    }

-- | Valores-base 
width, height, baseSize, alienWidth, bulletSize :: Num a => a
width = 200
height = 200
baseSize = 10
alienWidth = 20
bulletSize = 5

alienQtty :: Int
alienQtty = 44

-- | PLAYER ------------------------------------------------------------
createPlayer :: Player
createPlayer = (0, color green p)
    where
        p = pictures [tank, cannon]
        tank   = rectangleSolid (3 * baseSize) baseSize
        cannon = translate 0 5 $ rectangleSolid (baseSize / 2) baseSize

getPlayerPosition :: Player -> Float
getPlayerPosition (x, _) = x

drawPlayer :: Player -> Picture
drawPlayer (x, p) = translate x (-280) p

updatePlayerPosition :: Float -> Player -> Player
updatePlayerPosition direction (x, p)
    | x + s < - adjX || x + s > adjX = (x, p)
    | otherwise = (x + s, p)
    where
        adjX = width - baseSize * 3 / 2
        s = 10 * direction


-- | BULLETS ------------------------------------------------------------
createBullet :: Float -> Float -> Float -> Color -> Float -> Bullet
createBullet x y vy c size = Bullet ((x, y), (0, vy), color c $ rectangleSolid size (2*size))

getBulletPosition :: Bullet -> Maybe (Float, Float)
getBulletPosition (Bullet ((x, y), _, _)) = Just (x, y)
getBulletPosition Spent                   = Nothing

drawBullet :: Bullet -> Picture
drawBullet (Bullet ((x, y), _, p)) = translate x y p
drawBullet Spent                   = blank

updateBulletPosition :: Float -> Bullet -> Bullet
updateBulletPosition _ Spent = Spent
updateBulletPosition dt (Bullet ((x, y), (vx, vy), p))
    | y < adjY && y > -adjY = Bullet ((x, y + vy * dt), (vx, vy), p)
    | otherwise             = Spent
    where
        adjY = 1.5 * height - bulletSize

updateBullet :: Float -> [Bullet] -> [Bullet]
updateBullet dt bs = filter (/= Spent) (map (updateBulletPosition dt) bs)


-- | ALIENS --------------------------------------------------------
createAlien :: Float -> Float -> Float -> Color -> Picture -> Alien
createAlien x y vx c alien = Alien ((x, y), (vx, 0), color c alien')
    where
        alien' = scale 0.01 0.01 alien

createAliens :: [Picture] -> Z.Zipper [Alien]
createAliens models = as
    where
        lineLimits  = [(-150), (-120) .. 150]
        aliensLine1 = [createAlien x y 25 white $ head models | x <- lineLimits, y <- [80]]
        aliensLine2 = [createAlien x y 25 white $ models !! 1 | x <- lineLimits, y <- [120]]
        aliensLine3 = [createAlien x y 25 white $ models !! 2 | x <- lineLimits, y <- [160]]
        aliensLine4 = [createAlien x y 25 white $ models !! 3 | x <- lineLimits, y <- [200]]
        as          = Z.fromList [aliensLine1, aliensLine2, aliensLine3, aliensLine4]

getAlienPosition :: Alien -> Maybe (Float, Float)
getAlienPosition (Alien ((x, y), _, _)) = Just (x, y)
getAlienPosition Landed                 = Just (0, 0)
getAlienPosition Destroyed              = Nothing

drawAlien :: Alien -> Picture
drawAlien (Alien ((x, y), _, p)) = translate x y p
drawAlien Destroyed              = blank
drawAlien Landed                 = blank

-- | Percorre o zipper até a linha correspondente e aplica as atualizações de posição e velocidade
applyChanges :: Z.Zipper [Alien] -> Int -> Float -> Float -> Z.Zipper [Alien]
applyChanges alien n s dt =
    Z.replace (fmap (updateAlienVelocity . updateAlienPosition s dt) (Z.cursor shifted)) shifted
    where
        shifted = goRightNTimes alien n

updateAlien :: Int -> Float -> Float -> Z.Zipper [Alien] -> Z.Zipper [Alien]
updateAlien n s dt as = applyChanges as n s dt

updateAlienPosition :: Float -> Float -> Alien -> Alien
updateAlienPosition _ _ Destroyed = Destroyed
updateAlienPosition _ _ Landed = Landed
updateAlienPosition s dt (Alien ((x, y), v@(vx, _), p))
    | limit     = Alien ((x, y - 2), (0, 0), p)
    | otherwise = Alien ((x + vx * s * dt, y), v, p)
    where
        limit = y < -adjY + 60 && y > -adjY + 20
        adjY  = 1.5 * height - alienWidth / 2

updateAlienVelocity :: Alien -> Alien
updateAlienVelocity Destroyed = Destroyed
updateAlienVelocity Landed    = Landed
updateAlienVelocity (Alien b@((x, y), (vx, vy), p))
        | x < -adjX      = Alien ((-adjX, y - 20), (-vx, vy), p)
        | x > adjX       = Alien ((adjX, y - 20), (-vx, vy), p)
        | limit1         = Alien ((x, y - 20), (0, vy), p)
        | limit2         = Landed
        | otherwise      = Alien b
    where
        limit1 = y < -adjY + 60 && y > -adjY + 20
        limit2 = y < -adjY + 20
        adjX  = width - alienWidth / 2
        adjY  = 1.5 * height - alienWidth / 2

-- | Geração de número no intervalo 0 - quantidade atual de Aliens
getRandomNumber :: [Alien] -> StdGen -> (Int, StdGen)
getRandomNumber as = randomR (0, length as)

-- | Seleção de um Alien
selectAlien :: [Alien] -> Int -> Alien
selectAlien alien pos
    | pos < length alien = alien !! pos
    | otherwise          = alien !! (length alien - 1)

-- | Gera Bullet na posição de algum Alien aleatório
randomBulletPosition :: Z.Zipper [Alien] -> StdGen -> ([Bullet], StdGen)
randomBulletPosition as g = ([createBullet x y (-150) white bulletSize], snd rng)
    where
        concatAliens = concat $ Z.toList as
        Just (x, y)  = getAlienPosition $ selectAlien concatAliens $ fst rng
        rng          = getRandomNumber concatAliens g


-- BARRIER ------------------------------------------
createBarrier :: Float -> Float -> Color -> [Barrier]
createBarrier x y c = barrier
    where
        bs      = baseSize
        b1      = [Barrier ((x - 2 * bs, y), color c $ rectangleSolid baseSize baseSize)]
        b2      = [Barrier ((x', y'), color c $ rectangleSolid baseSize baseSize) | x' <- [x - 2 * bs, x - bs], y' <- [y + bs]]
        b3      = [Barrier ((x', y'), color c $ rectangleSolid baseSize baseSize) | x' <- [x - 2 * bs, x - bs ..  x + 2 * bs], y' <- [y + 2 * bs]]
        b4      = [Barrier ((x', y'), color c $ rectangleSolid baseSize baseSize) | x' <- [x + 2 * bs, x + bs], y' <- [y + bs]]
        b5      = [Barrier ((x + 2 * bs, y), color c $ rectangleSolid baseSize baseSize)]
        barrier = b1 ++ b2 ++ b3 ++ b4 ++ b5

createBarriers :: Z.Zipper [Barrier]
createBarriers = bs
    where 
        b1 = createBarrier (-80) (-260) green
        b2 = createBarrier 80 (-260) green
        bs = Z.fromList [b1, b2] 

getBarrierPosition :: Barrier -> Maybe (Float, Float)
getBarrierPosition (Barrier ((x, y), _)) = Just (x, y)
getBarrierPosition ShotDown              = Nothing

drawBarrier :: Barrier -> Picture
drawBarrier (Barrier ((x, y), p)) = translate x y p
drawBarrier ShotDown              = blank


-- MODELO DE COLISÕES (Baseado no projeto Haskeroids - https://haskell.pesquisa.ufabc.edu.br/cursos/21.q2.haskell/#orgf941bef) -----------------
-- | Verifica o contato entre um Alien e uma Bullet, marcando-o de acordo (False: contato, True: sem contato)
updateStateAlien :: Alien -> [Bullet] -> (Alien, Bool)
updateStateAlien alien [] = (alien, True)
updateStateAlien alien (b:bs) =
    case getAlienPosition alien of
        Nothing -> (alien, False)
        Just (x, y) -> case getBulletPosition b of
            Nothing -> (alien, True)
            Just (x', y') -> if collision then (Destroyed, False) else updateStateAlien alien bs
                where
                    collision = x' >= x - margin && x' <= x + margin && y' >= y - 2 * margin' && y' <= y + 2 * margin'
                    margin    = alienWidth / 2
                    margin'   = baseSize / 2

-- | Verifica o contato entre uma Barrier e uma Bullet, marcando-a de acordo (False: contato, True: sem contato)
updateStateBarrier :: Barrier -> [Bullet] -> (Barrier, Bool)
updateStateBarrier barrier [] = (barrier, True)
updateStateBarrier barrier (b:bs) =
    case getBarrierPosition barrier of
        Nothing -> (barrier, False)
        Just (x, y) -> case getBulletPosition b of
            Nothing -> (barrier, True)
            Just (x', y') -> if collision then (ShotDown, False) else updateStateBarrier barrier bs
                where
                    collision = x >= x' - margin && x <= x' + margin && y >= y' - margin' && y <= y' + margin'
                    margin    = baseSize / 2
                    margin'   = baseSize / 2

-- | Verifica o contato entre uma Bullet e um Alien, marcando-a de acordo (False: contato, True: sem contato)
updateStateBullet :: Bullet -> [Alien] -> (Bullet, Bool)
updateStateBullet bullet [] = (bullet, True)
updateStateBullet bullet (a:as) =
    case getBulletPosition bullet of
        Nothing -> (Spent, False)
        Just (x, y) -> case getAlienPosition a of
            Nothing -> (bullet, True)
            Just (x', y') -> if collision then (Spent, False) else updateStateBullet bullet as
                where
                    collision = x >= x' - margin && x <= x' + margin && y >= y' - 2 * margin' && y <= y' + 2 * margin'
                    margin    = alienWidth / 2
                    margin'   = baseSize / 2

-- | Verifica o contato entre uma Bullet e uma Barrier, marcando-a de acordo (False: contato, True: sem contato)
updateStateBullet' :: Bullet -> [Barrier] -> (Bullet, Bool)
updateStateBullet' bullet [] = (bullet, True)
updateStateBullet' bullet (b:brs) =
    case getBulletPosition bullet of
        Nothing -> (Spent, False)
        Just (x, y) -> case getBarrierPosition b of
            Nothing -> (bullet, True)
            Just (x', y') -> if collision then (Spent, False) else updateStateBullet' bullet brs
                where
                    collision = x >= x' - margin && x <= x' + margin && y >= y' - margin' && y <= y' + margin'
                    margin    = baseSize / 2
                    margin'   = baseSize / 2

-- | Repassa matriz de Alien atualizada com os estados de colisão
updateStateAlienList :: [[Alien]] -> [Bullet] -> [[(Alien, Bool)]]
updateStateAlienList as bs = map (map (`updateStateAlien` bs)) as

-- | Repassa matriz de Barrier atualizada com os estados de colisão
updateStateBarrierList :: [[Barrier]] -> [Bullet] -> [[(Barrier, Bool)]]
updateStateBarrierList brs bs = map (map (`updateStateBarrier` bs)) brs

-- | Repassa lista de Bullet atualizada com os estados de colisão
updateStateBulletList :: [Bullet] -> [Alien] -> [(Bullet, Bool)]
updateStateBulletList bs as = map (`updateStateBullet` as) bs

-- | Repassa lista de Bullet atualizada com os estados de colisão
updateStateBulletList' :: [Bullet] -> [Barrier] -> [(Bullet, Bool)]
updateStateBulletList' bs brs = map (`updateStateBullet'` brs) bs

-- | Forma uma tupla com as lista de estados de colisão
updateStateLists :: [[Alien]] -> [Bullet] -> ([[(Alien, Bool)]], [(Bullet, Bool)])
updateStateLists as bs = (alienCheck, bulletCheck)
    where
        alienCheck  = updateStateAlienList as bs
        bulletCheck = updateStateBulletList bs (concat as)

-- | Forma uma tupla com as lista de estados de colisão
updateStateLists' :: [[Barrier]] -> [Bullet] -> ([[(Barrier, Bool)]], [(Bullet, Bool)])
updateStateLists' b bs = (barrierCheck, bulletCheck)
    where
        barrierCheck = updateStateBarrierList b bs
        bulletCheck  = updateStateBulletList' bs (concat b)

-- | Checa colisão entre Player e Bullet inimiga
checkCollisionPlayer :: Player -> [Bullet] -> Bool
checkCollisionPlayer _ [] = False
checkCollisionPlayer p@(x, _) (b:bs) = case getBulletPosition b of
    Nothing     -> checkCollisionPlayer p bs
    Just (x', y') -> collision || checkCollisionPlayer p bs
        where
            collision = x' >= x - margin && x' <= x + margin && y' >= (-280) - 2 * margin' && y' <= (-280) + 2 * margin'  
            margin    = baseSize * 3 / 2
            margin'   = baseSize / 2

-- | Filtragem de linha de Aliens marcados com colisão
filterLine :: [(a, Bool)] -> [a]
filterLine l = map fst (filter snd l)

-- | Mapeamento da matriz para filtragem e retorno de zipper atualizado
filterAliens :: [[(Alien, Bool)]] -> Z.Zipper [Alien]
filterAliens as = Z.fromList $ map filterLine as

-- | Filtragem de Bullets marcadas com colisão
filterBullets :: [(Bullet, Bool)] -> [Bullet]
filterBullets bs = map fst (filter snd bs)

-- | Mapeamento da matriz para filtragem e retorno de zipper atualizado
filterBarriers :: [[(Barrier, Bool)]] -> Z.Zipper [Barrier]
filterBarriers brs = Z.fromList $ map filterLine brs

-- | Função principal de checagem por colisões entre Aliens e Bullets
checkCollision :: Z.Zipper [Alien] -> [Bullet] -> (Z.Zipper [Alien], [Bullet])
checkCollision as bs = (a'', b'')
    where
        (a', b') = updateStateLists (Z.toList as) bs
        a''      = filterAliens a'
        b''      = filterBullets b'

checkCollisionBarrier :: Z.Zipper [Barrier] -> [Bullet] -> (Z.Zipper [Barrier], [Bullet])
checkCollisionBarrier brs bs = (br'', b'')
    where
        (br', b') = updateStateLists' (Z.toList brs) bs
        br''      = filterBarriers br'
        b''       = filterBullets b'       

-- DESENHO DOS MODELOS --------------------------------------------------
-- | Desenha placar atualizado 
drawScore :: Int -> Picture
drawScore s = translate (-202) 310 scaled
    where
        scaled  = scale 0.125 0.125 colored
        colored = color (greyN 0.5) base
        base    = Text $ "Score: " ++ show s

-- | Desenha bordas estéticas em ambos os lados
drawBorders :: Picture
drawBorders = color white borders
    where
        borders = pictures [left, right]
        left    = translate (-200) 8.5 $ rectangleSolid 2 587.5
        right   = translate 200 8.5 $ rectangleSolid 2 587.5

-- | Desenha tutorial de teclas para comandos
drawKeys :: Picture
drawKeys = translate (-68) 310 scaled
    where
        scaled  = scale 0.1 0.1 colored
        colored = color (greyN 0.5) base
        base    = Text "Arrows: Move | Space: Shoot | Esc: Quit"

-- | Desenha informações de fim de jogo
gameOverInfo :: Bool -> Bool -> Picture
gameOverInfo g w =
    if g then pictures [title, key]
    else blank
    where
        title    = translate (-70) 100 scaled
        key      = translate (-110) 60 scaled'
        scaled   = scale 0.2 0.2 colored
        scaled'  = scale 0.125 0.125 colored'
        colored  = color (greyN 0.5) base
        colored' = color (greyN 0.5) base'
        base     = if w then Text "You Won!" else Text "Game Over"
        base'    = Text "Press Enter to play again"

-- | Desenho dos modelos um a um e combinação
drawModels :: WorldState -> Picture
drawModels ws = pictures $
    aliensPic
    ++ playerPic
    ++ barriersPic
    ++ bulletsPic
    ++ alienBulletsPic
    ++ scorePic
    ++ bordersPic
    ++ keysPic
    ++ [gameOverInfo (gameOver ws) (winFlag ws)]
    where
        aliensPic       = fmap draw (concat $ Z.toList $ aliens ws)
        playerPic       = [drawPlayer $ player ws]
        barriersPic     = fmap draw (concat $ Z.toList $ barriers ws)
        bulletsPic      = map draw (fst $ alienBullets ws)
        alienBulletsPic = map draw (bullets ws)
        scorePic        = [drawScore (score ws)]
        bordersPic      = [drawBorders]
        keysPic         = [drawKeys]


-- GERENCIADOR DE EVENTOS COM INTERAÇÃO DO USUÁRIO ----------------------
eventHandler :: Event -> WorldState -> WorldState
eventHandler (EventKey (SpecialKey x) Down _ _) ws = case x of
    KeyLeft  ->
        WorldState  (alienModel ws) (aliens ws) (alienLine ws)  player' (barriers ws) 
                    (bullets ws) (alienBullets ws) (speed ws) (score ws) (gameOver ws) (winFlag ws) (gameRound ws)

    KeyRight ->
        WorldState  (alienModel ws) (aliens ws) (alienLine ws) player'' (barriers ws) 
                    (bullets ws) (alienBullets ws) (speed ws) (score ws) (gameOver ws) (winFlag ws) (gameRound ws)

    KeySpace -> if not $ gameOver ws
        then    WorldState (alienModel ws) (aliens ws) (alienLine ws) (player ws) (barriers ws) 
                bullets' (alienBullets ws) (speed ws) (score ws) (gameOver ws) (winFlag ws) (gameRound ws)
        else ws

    KeyEnter -> if gameOver ws
        then    WorldState (alienModel ws) aliensDef alienLineDef playerDef barriersDef
                (bullets ws) (alienBullets ws) (speed ws) (score ws) gameOverDef winFlagDef gameRoundDef
        else ws

    _ -> ws
    where
        player'  = updatePlayerPosition (-1) (player ws)
        player'' = updatePlayerPosition 1 (player ws)
        bullets' = if null (bullets ws)
            then createBullet (getPlayerPosition $ player ws) (-270) 350 green bulletSize : bullets ws
            else bullets ws

        -- Redefinindo posições iniciais para o caso de reinício
        aliensDef    = createAliens $ alienModel ws
        alienLineDef = 0
        playerDef    = createPlayer
        barriersDef  = createBarriers
        gameOverDef  = False
        winFlagDef   = False
        gameRoundDef = if winFlag ws then gameRound ws + 1 else 0
eventHandler _ ws = ws

-- | Verificação de linha por Alien que chegou ao fim da tela
checkLine :: [Alien] -> Bool
checkLine [] = False
checkLine (a:as)
    | a == Landed = True
    | otherwise   = checkLine as

-- | Verificação da matriz de Alien para fim de jogo
checkLanding :: [[Alien]] -> Bool
checkLanding = any ((==True) . checkLine)


-- UPDATE -----------------------------------
-- | Modelo de fim de jogo
gameOverWorld :: WorldState -> StdGen -> WorldState
gameOverWorld ws rng = 
    WorldState  (alienModel ws) (Z.fromList []) 0 (0, blank) (Z.fromList []) 
                [] ([], rng) 0 (score ws) True (winFlag ws) (gameRound ws)

-- | Atualizador de modelo a cada frame 
updateModel :: Float -> WorldState -> WorldState
updateModel dt ws
    | gameOver ws = 
        gameOverWorld ws (snd alienBullets'') 
    | otherwise = 
        WorldState (alienModel ws) aliens'' alienLine' player' barriers''
                    bullets''' alienBullets'' speed' score' gameOver' winFlag' (gameRound ws)
    where
        -- Atualiza zipper e retorna a posição inicial
        aliens'         = goLeftNTimes (updateAlien (alienLine ws) (speed ws) dt (aliens ws)) 3
        bullets'        = updateBullet dt (bullets ws)
        
        -- Checa colisão projéteis do jogador com barreiras
        bundle          = checkCollisionBarrier (barriers ws) bullets'
        barriers'       = fst bundle
        bullets''       = snd bundle
        
        -- Checa colisão projéteis de aliens com barreiras
        bundle'         = checkCollisionBarrier barriers' (fst $ alienBullets ws)
        barriers''      = fst bundle'
        alienBullets'   = snd bundle'

        -- Checa colisão entre projéteis do jogador e aliens
        bundle''        = checkCollision aliens' bullets''
        aliens''        = fst bundle''
        alienLine'      = (alienLine ws + 1) `mod` 4
        player'         = player ws
        bullets'''      = snd bundle''
        alienBullets''  = if null (fst $ alienBullets ws)
                          then randomBulletPosition aliens'' (snd $ alienBullets ws)
                          else (updateBullet dt alienBullets', snd $ alienBullets ws)

        remainingAliens = length $ concat $ Z.toList aliens''

        -- Atualização de velocidade conforme número de Aliens diminui para aumentar dificuldade
        speed'
            | remainingAliens < alienQtty && remainingAliens >= tier1 = 2 + 0.3 * coef
            | remainingAliens < tier1 && remainingAliens >= tier2     = 2 + 0.4 * coef
            | remainingAliens < tier2 && remainingAliens > 6          = 2 + 0.5 * coef
            | remainingAliens <= 6                                    = 2 + 0.6 * coef
            | otherwise                                               = 2
            where
                tier1   = alienQtty `div` 2
                tier2   = alienQtty `div` 4
                coef    = fromIntegral (alienQtty - remainingAliens)

        score'          = (alienQtty - remainingAliens) * 10 + alienQtty * 10 * gameRound ws
        
        -- Condicionais para fim de jogo, nos casos de colisão com Player, eliminação de Aliens ou pouso de um Alien
        gameOver'       = checkCollisionPlayer player' (fst alienBullets'') || winFlag' || checkLanding (Z.toList aliens'')
        winFlag'        = remainingAliens == 0


-- Função-base ----------------------------------------------------------
runGame :: IO ()
runGame = do
    alien  <- loadBMP "./src/assets/alien.bmp"
    alien2 <- loadBMP "./src/assets/alien2.bmp"
    alien3 <- loadBMP "./src/assets/alien3.bmp"
    alien4 <- loadBMP "./src/assets/alien4.bmp"
    g      <- newStdGen
    let
        -- Definição dos parâmetros iniciais
        alienModelDef   = [alien, alien2, alien3, alien4]
        aliensDef       = createAliens alienModelDef
        alienLineDef    = 0
        playerDef       = createPlayer
        barriersDef     = createBarriers
        bulletsDef      = []
        alienBulletsDef = ([], g)
        speedDef        = 2
        scoreDef        = 0
        gameOverDef     = False
        winFlagDef      = False
        gameRoundDef    = 0
        initialWorld    =
            WorldState
                alienModelDef
                aliensDef
                alienLineDef
                playerDef
                barriersDef
                bulletsDef
                alienBulletsDef
                speedDef
                scoreDef
                gameOverDef
                winFlagDef
                gameRoundDef
    -- Execução do jogo
    play
        FullScreen
        (greyN 0.1)
        60
        initialWorld
        drawModels
        eventHandler
        updateModel
