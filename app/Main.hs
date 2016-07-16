module Main where

import           Control.Arrow
import           Lifx
import           System.Directory
import           System.Environment (getArgs, getProgName)
import           System.Exit        (exitFailure)
import           System.IO          (hPutStrLn, stderr)
import           System.IO.Error    (tryIOError)
import           Data.ByteString   as BS (ByteString, readFile)


main :: IO ()
main = do
  args <- getArgs
  lfxtoken <- lifxToken
  name <- getProgName
  case (args, lfxtoken) of
      (["token", lfxtoken], _) -> do
          home <- getHomeDirectory
          writeFile (home ++ "/.lfxtoken") lfxtoken
          putStrLn "LIFX token written to ~/.lfxtoken"
      (_, Nothing) ->
          hPutStrLn stderr $ "You need to set token first: " ++ name ++ " token <token>"
      (["ls"], Just token) -> do
          Right lights <- listLights token
          mapM_ (\x -> putStrLn $ Lifx.id x ++ " :: " ++ label x) lights
      (["toggle"], Just token) -> do
              toggleLights token "all"
              putStrLn "toggle all :: ok"
      (["toggle", selector], Just token) -> do
          toggleLights token selector
          putStrLn $ "toggle " ++ selector ++ " :: ok"
      _ -> do
          hPutStrLn stderr $ "usage: " ++ name ++ " [ls | toggle :selector]"
          exitFailure

lifxToken :: IO (Maybe BS.ByteString)
lifxToken = do
      home <- getHomeDirectory
      token <- tryIOError $ BS.readFile $ home ++ "/.lfxtoken"
      return $ either (const Nothing) Just token
