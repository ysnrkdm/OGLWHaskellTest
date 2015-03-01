--
-- Original source from https://github.com/madjestic/Haskell-OpenGL-Tutorial/blob/master/tutorial04/NGL/LoadShaders.hs
-- by madjestic/Haskell-OpenGL-Tutorial (GitHub)
--

module LoadShaders where

import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL.Raw.Core31 (glBindAttribLocation)
import Graphics.Rendering.OpenGL (($=))

--------------------------------------------------------------------------------

-- | The source of the shader source code.

data ShaderSource =
     ByteStringSource B.ByteString
     -- ^ The shader source code is directly given as a 'B.ByteString'.
   | StringSource String
     -- ^ The shader source code is directly given as a 'String'.
   | FileSource FilePath
     -- ^ The shader source code is located in the file at the given 'FilePath'.
   deriving ( Eq, Ord, Show )

getSource :: ShaderSource -> IO B.ByteString
getSource (ByteStringSource bs) = return bs
getSource (StringSource str) = return $ packUtf8 str
getSource (FileSource path) = B.readFile path

packUtf8 :: String -> B.ByteString
packUtf8 = TE.encodeUtf8 . T.pack

--------------------------------------------------------------------------------

-- | A description of a shader: The type of the shader plus its source code.

data ShaderInfo = ShaderInfo GL.ShaderType ShaderSource String GL.AttribLocation
   deriving ( Eq, Ord, Show )

--------------------------------------------------------------------------------

-- | Create a new program object from the given shaders, throwing an
-- 'IOException' if something goes wrong.

loadShaders :: [ShaderInfo] -> IO GL.Program
loadShaders infos =
    GL.createProgram `bracketOnError` GL.deleteObjectName $ \program -> do
        putStrLn "Load&compile,attach..."
        loadCompileAttach program infos
        putStrLn "Done. Link&check..."
        linkAndCheck program
        putStrLn "Done. Returning."
        return program

linkAndCheck :: GL.Program -> IO ()
linkAndCheck = checked GL.linkProgram GL.linkStatus GL.programInfoLog "link"

loadCompileAttach :: GL.Program -> [ShaderInfo] -> IO ()
loadCompileAttach _ [] = return ()
loadCompileAttach program (ShaderInfo shType source attribName attribLocation : infos) =
    GL.createShader shType `bracketOnError` GL.deleteObjectName $ \shader -> do
        putStrLn "Getting source"
        src <- getSource source
        putStrLn "ShaderSourceBS"
        putStrLn $ show src
        GL.shaderSourceBS shader $= src
        putStrLn "CompileAndCheck"
        compileAndCheck shader
        putStrLn "AttachShader"
        GL.attachShader program shader
        putStrLn "loadCompileAttach next"
        -- Bind attribute locations.
        -- This needs to be done prior to linking.
        GL.attribLocation program attribName $= attribLocation
        loadCompileAttach program infos

compileAndCheck :: GL.Shader -> IO ()
compileAndCheck = checked GL.compileShader GL.compileStatus GL.shaderInfoLog "compile"

checked :: Show t => (t -> IO ())
        -> (t -> GL.GettableStateVar Bool)
        -> (t -> GL.GettableStateVar String)
        -> String
        -> t
        -> IO ()
checked action getStatus getInfoLog message object = do
    action object
    ok <- GL.get (getStatus object)
    unless ok $ do
        infoLog <- GL.get (getInfoLog object)
        fail (message ++ " log: " ++ infoLog ++ ". Object used is: " ++ (show object))