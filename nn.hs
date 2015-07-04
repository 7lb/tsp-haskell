import Control.Monad (replicateM, fmap, forM_)
import Control.Parallel
import Data.IORef
import Data.List ((\\))
import Graphics.UI.GLUT
import System.Random (getStdRandom, randomR,
                      setStdGen, mkStdGen, getStdGen)
import System.Random.Shuffle (shuffle')
import qualified Data.Map as Map (findMax, findMin, fromList)

initSeed  = 997
numCities = 30

popSize   = ceiling (fromIntegral numCities * 1.5)
mutRate   = 0.03
elitism   = ceiling (fromIntegral popSize / 10)

debug     = False

type City       = (Float, Float)
type Tour       = [City]
type Population = [Tour]
type GLPoint    = (GLfloat, GLfloat, GLfloat)

randFloat :: Float -> Float -> IO Float
randFloat a b = getStdRandom . randomR $ (a, b)

randInt :: Int -> Int -> IO Int
randInt a b = getStdRandom . randomR $ (a, b)

randomCity :: IO City
randomCity = do
    r1 <- randFloat 0 1
    r2 <- randFloat 0 1
    return (r1, r2)

randomTour :: Int -> IO Tour
randomTour 0 = return []
randomTour n = do
    rc <- randomCity
    rt <- randomTour (n - 1)
    return (rc : rt)

shuffleTour :: Tour -> IO Tour
shuffleTour [] = return []
shuffleTour t = do
    g  <- getStdGen
    return $ shuffle' t (length t) g

randomizedPop :: Int -> Tour -> IO Population
randomizedPop 0 _ = return []
randomizedPop n t = do
    t' <- shuffleTour t
    ts <- randomizedPop (n - 1) t'
    return (t' : ts)

distance :: City -> City -> Float
distance (x1, y1) (x2, y2) = sqrt $ (x2 - x1)^2
                                  + (y2 - y1)^2

tourDistance :: Tour -> Float
tourDistance t@(x:xs) = sum $ zipWith distance t t'
    where t' = xs ++ [x]

fitness :: Tour -> Float
fitness = (1/) . tourDistance

select :: Population -> IO Tour
select [] = return []
select p  = do
    i <- randInt 0 (length p - 1)
    r <- randFloat 0 1
    if r < fitness (p !! i) / (maximum . map fitness $ p)
    then return (p !! i)
    else select p

crossover :: Tour -> Tour -> IO Tour
crossover [] _ = return []
crossover _ [] = return []
crossover m f  = do
    i <- randInt 0 (length m - 1)
    j <- randInt i (length m - 1)
    let s = sub i j f
    return $ take i (m\\s) ++ s ++ drop i (m\\s)

sub :: Int -> Int -> [a] -> [a]
sub _ _ [] = []
sub i j t
    | i > j     = sub j i t
    | otherwise = drop i . take (j + 1) $ t

mutate :: Tour -> IO Tour
mutate [] = return []
mutate t  = do
    i <- randInt 0 (length t - 1)
    j <- randInt 0 (length t - 1)
    c <- randFloat 0 1
    if c < mutRate
    then return . swap i j $ t
    else return t

swap :: Int -> Int -> [a] -> [a]
swap _ _ [] = []
swap i j l  = map t $ zip [0..] l
    where t (k, v)
              | k == i    = l !! j
              | k == j    = l !! i
              | otherwise = v

generateNewPop :: Int -> Population -> IO Population
generateNewPop _ [] = return []
generateNewPop 0 _  = return []
generateNewPop n p  = do
    m  <- select p
    f  <- select p
    c' <- crossover m f
    c  <- fmap last . replicateM numCities $ mutate c'
    cs <- generateNewPop (n - 1) p
    return (c : cs)

elite :: Int -> Population -> Population
elite _ [] = []
elite 0 _  = []
elite n p  =  best : elite (n - 1) (p\\[best])
    where best = snd . Map.findMax
                     . Map.fromList
                     . makeMap $ p

makeMap :: Population -> [(Float, Tour)]
makeMap []     = []
makeMap (t:ts) = (fitness t, t) `par` makeMap ts
                 `pseq` (fitness t, t) : makeMap ts

evolve :: Population -> IO Population
evolve p = do
    let bestTours = elite elitism p
    np <- generateNewPop (popSize - elitism) p
    return (np ++ bestTours)

nearest :: Tour -> Tour
nearest [] = []
nearest (x:[]) = [x]
nearest (x:xs) = x : nearest (snd best : (xs\\[snd best]) )
    where best = Map.findMin (Map.fromList $ zip (map (distance x) xs) xs)

altPop :: Int -> Tour -> Population
altPop 0 _ = []
altPop n t = k : altPop (n - 1) t
    where k = nearest (take numCities . drop n . cycle $ t)

main :: IO ()
main = do
    (_progName, _args) <- getArgsAndInitialize
    _window <- createWindow "TSP"

    if debug == True
    then setStdGen (mkStdGen initSeed)
    else return ()

    destinationCities <- randomTour numCities
    let p1 = altPop numCities destinationCities
    p2 <- randomizedPop (popSize - numCities) destinationCities
    let p = p1 ++ p2

    pv  <- newIORef p
    gen <- newIORef 0

    idleCallback $= Just (idle pv gen)
    displayCallback $= (display pv gen)
    mainLoop

idle :: IORef Population -> IORef Int -> IdleCallback
idle pv gen = do
    p <- get pv
    pn <- evolve p
    pv $= pn
    gen $~! (+1)
    postRedisplay Nothing

display :: IORef Population -> IORef Int -> DisplayCallback
display pv gen = do
    clear [ColorBuffer]

    loadIdentity

    p <- get pv
    g <- get gen
    let bestT  = Map.findMax . Map.fromList . makeMap $ p
    let points = map cityToGLPoint (snd bestT)

    color $ Color3 0.6 0.6 (0.6 :: GLfloat)

    scale 0.9 0.9 (0.9 :: GLfloat)

    renderPrimitive LineLoop $
        mapM_ (\(x,y,z) -> vertex $ Vertex3 x y z) points

    forM_ points $ \(x, y, z) ->
        preservingMatrix $ do
            color $ Color3 0.4 0.8 (0.4 :: GLfloat)
            translate $ Vector3 x y z
            square 0.02

    translate $ Vector3 (- 1) 0.95 (0 :: GLfloat)
    scale 0.0005 0.0005 (0.0005 :: GLfloat)
    renderString MonoRoman ("Generation:" ++ show g
                         ++ " Distance:"
                         ++ show (1 / fst bestT))

    flush

cityToGLPoint :: City -> GLPoint
cityToGLPoint (a, b) = (x, y, 0)
    where x = (realToFrac a) * 2 - 1
          y = (realToFrac b) * 2 - 1

square :: GLfloat -> IO ()
square w = renderPrimitive Quads $ mapM_ vertex3f
    [ (-l, -l, 0), (l, -l, 0),
      ( l,  l, 0), (-l, l, 0) ]
    where l = w/2

vertex3f :: (GLfloat, GLfloat, GLfloat) -> IO ()
vertex3f (x, y, z) = vertex $ Vertex3 x y z
