module Main where

import System.Hardware.Serialport
import System.Environment
import System.Directory
import Data.List
import Control.Concurrent
import qualified Data.ByteString.Char8 as B

main :: IO ()
main = do
    a <- getArgs
    case a of
        [] -> putStrLn "Can't send empty message"
        (m:_) -> do
            c <- getCom
            s <- openSerial c $ defaultSerialSettings
                { commSpeed = CS9600 }
            send s (B.pack m)
            flush s
            threadDelay $ round 50e3
            x <- recv s 9
            B.putStrLn x

getCom :: IO String
getCom = do
    devs <- getDirectoryContents "/dev"
    return $ getUSB devs
    where
    getUSB :: [ String ] -> String
    getUSB s | found s == [] = error "No hardware present!"
             | otherwise = "/dev/" ++ ( head $ found s )
    found = filter (isPrefixOf "ttyUSB")
