module Main where
import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import qualified Data.Vector as V
import qualified Data.Matrix as M
import qualified MatrixHelper as MH

main = do
    defaultMain $ hUnitTestToTests $ TestList [ TestLabel "testInverse" $ TestCase testInverse,
                                                TestLabel "testInverse2" $ TestCase testInverse2,
                                                TestLabel "testVectorNormalize" $ TestCase testVectorNormalize,
                                                TestLabel "testCosSin" $ TestCase testCosSin,
                                                TestLabel "testMatProd" $ TestCase testMatProd]

testInverse = do
    let a = M.fromLists [[1,2,3,4],[5,6,7,8],[-10,2,-5,6],[5.5,4.3,(-2.4),9]]
    let inva = MH.inverse a
    let a_inva = M.multStd a inva
    a_inva @=? M.identity 4

testInverse2 = do
    let b = M.fromLists [[3, 1, 1, 2], [5, 1, 3, 4], [2, 0, 1, 0], [1, 3, 2, 1]]
    let invb = MH.inverse b
    let b_invb = M.multStd b invb
    putStrLn (show invb)
    b_invb @=? M.identity 4

testVectorNormalize = do
    let va = V.fromList [1, 2, 3]
    let nva = MH.vectorNormalize va
    nva @=? V.fromList [1 / 3.74166, 2 / 3.74166, 3 / 3.74166]

testCosSin = do
--    (cos 0) @=? (cos 3.14145256 * 2)
    (cos 0) @=? (cos 3.14145256 * 3)
    (cos 0) @=? (cos 3.14145256 * 4)

testMatProd = do
--    let a = M.fromLists [[1,2,3,4], [5,6,7,8], [9,10,11,12], [13,14,15,16]]
    let sa = M.fromLists [[1,2,3,4],[1,2,3,4],[1,2,3,4],[1,2,3,4]]
    let sb = M.fromLists [[5,6,7,8],[5,6,7,8],[5,6,7,8],[5,6,7,8]]
    putStrLn $ show $ sa * sb
    putStrLn $ show $ sb * sa
    putStrLn $ show $ M.multStd sa sb
    putStrLn $ show $ M.multStd sb sa
