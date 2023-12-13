{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Control.Concurrent       ( getNumCapabilities )
import Control.Concurrent.Async ( replicateConcurrently_ )
import Control.Concurrent.MVar  ( MVar, takeMVar, putMVar, newMVar )

import Control.Monad            ( forM_, when )

import GHC.Clock                ( getMonotonicTime )
import System.Posix qualified as Unix

(</>) :: FilePath -> FilePath -> FilePath
a </> b = a ++ "/" ++ b

forceList :: [a] -> ()
forceList = foldr seq ()

createLoop :: MVar Int
           -> (Int -> Int -> IO ())
           -> IO ()
createLoop mvar f = go
  where
    go = do
        n <- takeMVar mvar
        let
            n' = max 0 (n - 40)
            n'' = n - n'
        putMVar mvar (n - n'')
        forM_ [n, n - 1 .. n' + 1] (f n)
        when (n'' > 0) go

creatSym :: FilePath
         -> Int
         -> Int
         -> IO ()
creatSym dir n a = do
            let file = dir ++ "/" ++ show a ++ ".file"
                old = if n == a then 0 else (a + 1)
                fileOld = show old ++ ".file"
            b <- Unix.createSymbolicLink fileOld file
            c <- Unix.getFileStatus file
            -- d <- Unix.removeLink file
            return $ b `seq` c `seq` () `seq` ()

main :: IO ()
main = do
    let amount = 1000000

    cpus <- getNumCapabilities

    dir <- Unix.mkdtemp "too-many.tmp."
    fd <- Unix.createFile (dir </> "0.file") Unix.stdFileMode
    fd `seq` Unix.closeFd fd

    mvar <- newMVar amount
    start <- getMonotonicTime

    putStrLn $ "starting with threads:" ++ show cpus ++ " dir: " ++ dir

    replicateConcurrently_ cpus $ createLoop mvar $ creatSym dir

    end <- getMonotonicTime

    let time = end - start
        kiops :: Double
        kiops = fromIntegral amount / time * 1000

    putStrLn $ "kiops: " ++ show kiops

    Unix.removeDirectory dir
