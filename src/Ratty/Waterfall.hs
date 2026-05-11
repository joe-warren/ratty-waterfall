module Ratty.Waterfall where

import qualified Waterfall as W
import System.IO.Unsafe (unsafePerformIO)
import System.Random (randomRIO)
import Data.List (intercalate)
import Data.IORef
import System.Console.ANSI
import Control.Monad (forM_) 
import System.Environment (getEnv)

apcCommand :: String -> String-> String -> [(String, String)] -> String
apcCommand namespace thingy verb keyvalues = 
    let kv (k,v) = k <> "=" <> v
    in "\x1B_" 
        <> intercalate ";" (
                [namespace, thingy, verb] 
                    <> (kv <$> keyvalues)
            ) 
        <> "\x1B\\"

rattyCommand :: String -> [(String, String)] -> String
rattyCommand = apcCommand "ratty" "g"

{-# INLINE lastIDRef #-}
lastIDRef :: IORef (Maybe String)
lastIDRef = unsafePerformIO (newIORef Nothing)

vLines = 10

scale ::  W.Solid -> W.Solid
scale s = 
    case W.axisAlignedBoundingBox s of
        Nothing -> s
        Just (lo, hi) ->
            let biggestDim = max (abs (minimum lo)) (maximum hi)
            in W.uScale (0.25 / biggestDim) s


-- because of the way ratty tracks the position of objects
-- rather than printing newlines, print n random numbers in black
printNewLines :: Int -> IO ()
printNewLines i = do
    setSGR [SetColor Foreground Dull Black]
    forM_ [0..i] $ const $ do
        val <- show <$> randomRIO (0 :: Int, 10^(6::Int))
        putStrLn val
    setSGR [Reset]
instance Show W.Solid where
    show s = unsafePerformIO $ do
        ident <- show <$> randomRIO (0 :: Int, 10^(6::Int))
        let filename = ident <> ".glb"
        home <- getEnv "HOME"
        let path = home <> "/.cargo/bin/assets/objects/" <> filename
        W.writeGLB 0.01 path (scale s)
        oldIdent <- readIORef lastIDRef
        writeIORef lastIDRef (Just ident)
        -- createSpace
        printNewLines vLines
        putStr (cursorUpLineCode vLines)
        curPos <- getCursorPosition
        let curPosY = maybe 10 fst $ curPos
        putStr (cursorDownLineCode vLines)

        let register =
                rattyCommand "r" 
                    [("id", ident)
                    ,("fmt", "glb")
                    , ("path", filename)
                    ] 
        let draw =
                rattyCommand "p"
                    [("id", ident)
                    , ("row", show (curPosY + (vLines `div` 2)))
                    , ("col", "10")
                    , ("w", show vLines)
                    , ("h", "10")
                    , ("animate", "1")
                    ]
        let deletePrev' oldIdent' = rattyCommand "d" [("id", oldIdent')]
            deletePrev = foldMap deletePrev' oldIdent
        return $ register <> draw -- <> deletePrev
