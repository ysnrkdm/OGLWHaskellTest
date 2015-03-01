module MatrixHelper where

import qualified Data.Vector as V
import qualified Data.Matrix as M

radiansFromDegrees :: Double -> Double
radiansFromDegrees degrees = degrees * (pi / 180)

matrixPerspective :: Double -> Double -> Double -> Double -> M.Matrix Double
matrixPerspective fovyRadians aspect nearZ farZ =
    M.fromLists [ [cotan / aspect, 0.0,   0.0,                                   0.0],
                  [0.0,            cotan, 0.0,                                   0.0],
                  [0.0,            0.0,   (farZ + nearZ) / (nearZ - farZ),      -1.0],
                  [0.0,            0.0,   (2.0 * farZ * nearZ) / (nearZ - farZ), 0.0] ]
    where
        cotan = 1.0 / (tan (fovyRadians / 2.0))

matrixTranslation tx ty tz =
    M.fromLists [ [1.0, 0.0, 0.0, 0.0],
                  [0.0, 1.0, 0.0, 0.0],
                  [0.0, 0.0, 1.0, 0.0],
                  [ tx,  ty,  tz, 1.0] ]

matrixMakeRotation radians x y z =
    rotateMatrix
    where
    rotateMatrix = M.fromList 4 4 [ cosR + cosp * v0 * v0,
            cosp * v0 * v1 + v2 * sinR,
            cosp * v0 * v2 - v1 * sinR,
            0.0,
            cosp * v0 * v1 - v2 * sinR,
            cosR + cosp * v1 * v1,
            cosp * v1 * v2 + v0 * sinR,
            0.0,
            cosp * v0 * v2 + v1 * sinR,
            cosp * v1 * v2 - v0 * sinR,
            cosR + cosp * v2 * v2,
            0.0,
            0.0, 0.0, 0.0, 1.0 ]
            where
                cosR = cos radians
                cosp = 1.0 - cosR
                sinR = sin radians
                vector = vectorNormalize $ V.fromList [x, y, z]
                v0 = vector V.! 0
                v1 = vector V.! 1
                v2 = vector V.! 2

matrixRotate matrix radians x y z =
    rotateMatrix * matrix
    where
    rotateMatrix = matrixMakeRotation radians x y z

vectorNormalize :: Floating a => V.Vector a -> V.Vector a
vectorNormalize vector =
    V.map (\x -> x * scale) vector
    where
    scale = 1.0 / (sqrt $ V.sum $ V.map (\x -> x * x) vector)

cofactor :: Num a => M.Matrix a -> Int -> Int -> a
cofactor m i j = (-1)^(i+j) * M.detLaplace (M.minorMatrix i j m)

cofactorMatrix :: Num a => M.Matrix a -> M.Matrix a
cofactorMatrix m = M.fromLists [ [ (cofactor m i j) | j <- [1..n] ] | i <- [1..n] ]
    where
    n = M.nrows m

mapMatrix :: Num a => (a -> a) -> M.Matrix a -> M.Matrix a
mapMatrix f m = M.fromLists [ [ f $ m M.! (i,j) | j <- [1..M.ncols m] ] | i <- [1..M.nrows m] ]

inverse :: (Fractional a, Num a) => M.Matrix a -> M.Matrix a
inverse m = M.transpose $ mapMatrix (\x -> x / (M.detLaplace m)) (cofactorMatrix m)
