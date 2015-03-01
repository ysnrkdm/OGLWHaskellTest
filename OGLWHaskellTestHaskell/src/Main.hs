{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign.Ptr
import Control.Concurrent
import Control.Monad
import Data.IORef
import Foreign
import Foreign.C
import Graphics.Rendering.OpenGL.GL as GL
import Graphics.Rendering.OpenGL.Raw.Core31 (glUniformMatrix3fv, glUniformMatrix4fv)
import Text.Printf
import Data.Array.Storable (newListArray, withStorableArray)
import qualified Data.Matrix as M
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified MatrixHelper as MH
import qualified LoadShaders as LS

foreign import ccall safe "c_main" c_main :: FunPtr (Double -> Double -> Double -> IO ()) -> IO ()
foreign import ccall "wrapper" mkDrawFrame :: (Double -> Double -> Double -> IO ()) -> IO (FunPtr ((Double -> Double -> Double -> IO ())))
foreign import ccall safe "c_pathForResource" c_pathForResource :: CString -> CString -> IO CString
foreign export ccall loadShaders :: IO()

createVBO :: [GLfloat] -> IO BufferObject
createVBO elems = do
    [vertexBuffer] <- GL.genObjectNames 1
    GL.bindBuffer GL.ArrayBuffer $= Just vertexBuffer
    arr <- newListArray (0, len-1) elems
    let bufSize = toEnum $ len * sizeOf (head elems)
    withStorableArray arr $ \ptr ->
        GL.bufferData GL.ArrayBuffer $= (bufSize,ptr,GL.StaticDraw)
    GL.bindBuffer GL.ArrayBuffer $= Nothing
    return vertexBuffer
    where
        len = length elems

draw :: BufferObject -> VertexArrayObject -> IO ()
draw vertexBuffer vao = do
    GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled
    GL.vertexAttribArray (GL.AttribLocation 1) $= GL.Enabled
    GL.bindBuffer GL.ArrayBuffer               $= Just vertexBuffer
    GL.bindVertexArrayObject $= Just vao
    GL.vertexAttribPointer (GL.AttribLocation 0) $= (GL.ToFloat, pos_descriptor)
    GL.vertexAttribPointer (GL.AttribLocation 1) $= (GL.ToFloat, norm_descriptor)
    GL.drawArrays GL.Triangles 0 36
    GL.bindBuffer GL.ArrayBuffer $= Nothing
    GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Disabled
    GL.vertexAttribArray (GL.AttribLocation 1) $= GL.Disabled

    GL.flush
    where
        pos_descriptor = GL.VertexArrayDescriptor 3 GL.Float 24 nullPtr
            :: GL.VertexArrayDescriptor GLfloat
        norm_descriptor = GL.VertexArrayDescriptor 3 GL.Float 24 (plusPtr nullPtr 12)
            :: GL.VertexArrayDescriptor GLfloat

gCubeVertexData :: [GLfloat]
gCubeVertexData = [
    -- Data layout for each line below is:
    -- positionX, positionY, positionZ,     normalX, normalY, normalZ,
    0.5, -0.5, -0.5,        1.0, 0.0, 0.0,
    0.5, 0.5, -0.5,         1.0, 0.0, 0.0,
    0.5, -0.5, 0.5,         1.0, 0.0, 0.0,
    0.5, -0.5, 0.5,         1.0, 0.0, 0.0,
    0.5, 0.5, -0.5,          1.0, 0.0, 0.0,
    0.5, 0.5, 0.5,         1.0, 0.0, 0.0,

    0.5, 0.5, -0.5,         0.0, 1.0, 0.0,
    -0.5, 0.5, -0.5,        0.0, 1.0, 0.0,
    0.5, 0.5, 0.5,          0.0, 1.0, 0.0,
    0.5, 0.5, 0.5,          0.0, 1.0, 0.0,
    -0.5, 0.5, -0.5,        0.0, 1.0, 0.0,
    -0.5, 0.5, 0.5,         0.0, 1.0, 0.0,

    -0.5, 0.5, -0.5,        -1.0, 0.0, 0.0,
    -0.5, -0.5, -0.5,       -1.0, 0.0, 0.0,
    -0.5, 0.5, 0.5,         -1.0, 0.0, 0.0,
    -0.5, 0.5, 0.5,         -1.0, 0.0, 0.0,
    -0.5, -0.5, -0.5,       -1.0, 0.0, 0.0,
    -0.5, -0.5, 0.5,        -1.0, 0.0, 0.0,

    -0.5, -0.5, -0.5,       0.0, -1.0, 0.0,
    0.5, -0.5, -0.5,        0.0, -1.0, 0.0,
    -0.5, -0.5, 0.5,        0.0, -1.0, 0.0,
    -0.5, -0.5, 0.5,        0.0, -1.0, 0.0,
    0.5, -0.5, -0.5,        0.0, -1.0, 0.0,
    0.5, -0.5, 0.5,         0.0, -1.0, 0.0,

    0.5, 0.5, 0.5,          0.0, 0.0, 1.0,
    -0.5, 0.5, 0.5,         0.0, 0.0, 1.0,
    0.5, -0.5, 0.5,         0.0, 0.0, 1.0,
    0.5, -0.5, 0.5,         0.0, 0.0, 1.0,
    -0.5, 0.5, 0.5,         0.0, 0.0, 1.0,
    -0.5, -0.5, 0.5,        0.0, 0.0, 1.0,

    0.5, -0.5, -0.5,        0.0, 0.0, -1.0,
    -0.5, -0.5, -0.5,       0.0, 0.0, -1.0,
    0.5, 0.5, -0.5,         0.0, 0.0, -1.0,
    0.5, 0.5, -0.5,         0.0, 0.0, -1.0,
    -0.5, -0.5, -0.5,       0.0, 0.0, -1.0,
    -0.5, 0.5, -0.5,        0.0, 0.0, -1.0 ]

getMatrices :: Double -> Double -> (M.Matrix Double, M.Matrix Double)
getMatrices aspect rotation =
    (modelViewProjectionMatrix, normalMatrix)
    where
        modelViewProjectionMatrix = modelViewMatrix * projectionMatrix
        normalMatrix = M.transpose $ MH.inverse $ M.submatrix 1 3 1 3 modelViewMatrix
        projectionMatrix = MH.matrixPerspective (MH.radiansFromDegrees 65.0) aspect 0.1 100.0
        modelViewMatrix = modelViewMatrixE * baseModelViewMatrix
        baseModelViewMatrix = MH.matrixRotate (MH.matrixTranslation 0.0 0.0 (-4.0)) rotation 0.0 1.0 0.0
        modelViewMatrixE = MH.matrixRotate (MH.matrixTranslation 0.0 0.0 1.5) rotation 1.0 1.0 1.0

glFloatVectorFromMatrix3 :: M.Matrix Double -> VS.Vector GLfloat
glFloatVectorFromMatrix3 m =
    VS.fromList $ map realToFrac $ concat [ (V.toList (M.getRow 1 m33)),
        (V.toList (M.getRow 2 m33)),
        (V.toList (M.getRow 3 m33)) ]
    where
        m33 = M.submatrix 1 3 1 3 m

glFloatVectorFromMatrix4 :: M.Matrix Double -> VS.Vector GLfloat
glFloatVectorFromMatrix4 m =
    VS.fromList $ map realToFrac $ concat [ (V.toList (M.getRow 1 m44)),
        (V.toList (M.getRow 2 m44)),
        (V.toList (M.getRow 3 m44)),
        (V.toList (M.getRow 4 m44))]
    where
        m44 = M.submatrix 1 4 1 4 m

uniformLocate :: (GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
              -> VS.Vector GLfloat
              -> (Maybe GL.Program)
              -> String
              -> IO ()
uniformLocate uniformFn matrix (Just program) string = do
    loc <- GL.get $ GL.uniformLocation program string
    let (UniformLocation locId) = loc in
        VS.unsafeWith matrix $ \ptr -> uniformFn locId 1 0 ptr
uniformLocate uniformFn matrix Nothing string = do
    fail ("uniformLocate failed. Possibly program doesn't exist" )


drawFrame :: IORef Double -> Double -> Double -> Double -> IO()
drawFrame rotRef timeSinceLastUpdate width height = do
    GL.clearColor $= Color4 0.65 0.65 0.65 1.0
    GL.clear [GL.ColorBuffer,GL.DepthBuffer]

    -- Get, and bind VAO
    [vertexArrayId] <- GL.genObjectNames 1 :: IO [VertexArrayObject]
    vbo <- createVBO gCubeVertexData
    GL.bindVertexArrayObject $= Just vertexArrayId

    -- Calc position, normal and their matrices
    let aspect = (width / height)
    modifyIORef rotRef $ \x -> x + timeSinceLastUpdate * 0.5
    rotation <- readIORef rotRef

    let (modelViewProjectionMatrix, normalMatrix) = getMatrices aspect rotation

    mprog <- (GL.get GL.currentProgram)
    uniformLocate glUniformMatrix4fv (glFloatVectorFromMatrix4 modelViewProjectionMatrix) mprog "modelViewProjectionMatrix"
    uniformLocate glUniformMatrix3fv (glFloatVectorFromMatrix3 normalMatrix) mprog "normalMatrix"

    draw vbo vertexArrayId
    flush

loadShaders :: IO ()
loadShaders = do
    -- Load shaders
    vsFilePath <- withCString "Shader" $
        \shader -> withCString "vsh" $
            \vsh -> c_pathForResource shader vsh
    fsFilePath <- withCString "Shader" $
        \shader -> withCString "fsh" $
            \fsh -> c_pathForResource shader fsh

    vsFilePathStr <- peekCString vsFilePath
    fsFilePathStr <- peekCString fsFilePath

    putStrLn "Found below files:"
    putStrLn vsFilePathStr
    putStrLn fsFilePathStr

    putStrLn "Loading shaders from above files..."
    program <- LS.loadShaders [
        LS.ShaderInfo GL.VertexShader (LS.FileSource vsFilePathStr) "position" (GL.AttribLocation 0),
        LS.ShaderInfo GL.FragmentShader (LS.FileSource fsFilePathStr) "normal" (GL.AttribLocation 1)]

    putStrLn "Shaders loaded."
    putStrLn $ show program

    GL.currentProgram $= Just program

main = do
    putStrLn "Haskell start"
    rotRef <- newIORef 0.0

    drawFrame <- mkDrawFrame $ drawFrame rotRef
    c_main drawFrame