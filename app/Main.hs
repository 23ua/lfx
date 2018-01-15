module Main where

import           Control.Monad (void)
import           Data.Semigroup ((<>))
import           Lifx
import           Lifx.Command
import           System.Directory
import           System.Environment (getProgName)
import           System.IO          (hPutStrLn, stderr)
import           System.IO.Error    (tryIOError)
import           Data.ByteString   as BS (readFile, writeFile)
import Options.Applicative


main :: IO ()
main = handle' =<< customExecParser (prefs showHelpOnError) opts
    where
        opts = info (lfxParser <**> helper)
            ( fullDesc
            <> progDesc "You can control your lights via lan protocol or via internet"
            <> header "lfx -- cli-tool to control your LIFX smart lights"
            )

lfxParser :: Parser Opts
lfxParser = Opts <$>
    strOption
        ( long "selector"
            <> short 's'
            <> metavar "SELECTOR"
            <> value "all"
            <> help "Apply command to lights matching the SELECTOR"
        )
    <*> subparser
        ( command "on" (info (pure On) ( progDesc "Turn on the lights" ))
            <> command "off" (info (pure Off) ( progDesc "Turn off the lights" ))
            <> command "toggle" (info (pure Toggle) ( progDesc "Toggle the lights" ))
            <> command "ls" (info (pure Ls) ( progDesc "List the lights" ))
            <> command "brightness" (info brightnessOpt ( progDesc "Set the brightness of the lights" ))
            <> command "+" (info incrBrightnessOpt ( progDesc "Increase the brightness of the lights by PERCENT"))
            <> command "-" (info decrBrightnessOpt ( progDesc "Decrease the brightness of the lights by PERCENT"))
            <> command "color" (info colorOpt ( progDesc "Set COLOR for the lights"))
            <> command "token" (info setTokenOpt ( progDesc "Set the LIFX token"))
        )

brightnessOpt :: Parser Cmd
brightnessOpt = Brightness <$> argument auto (metavar "BRIGHTNESS_LEVEL")

incrBrightnessOpt :: Parser Cmd
incrBrightnessOpt = IncrBrightness <$> argument auto (metavar "PERCENT")

decrBrightnessOpt :: Parser Cmd
decrBrightnessOpt = DecrBrightness <$> argument auto (metavar "PERCENT")

colorOpt :: Parser Cmd
colorOpt = Lifx.Command.Color <$> argument str (metavar "COLOR")

setTokenOpt :: Parser Cmd
setTokenOpt = SetToken <$> argument str (metavar "TOKEN")

handle' :: Opts -> IO ()
handle' opts = do
    lfxtoken <- lifxToken
    name <- getProgName
    case (lfxtoken, opts) of
        (Just token, _) -> handle opts token
        (Nothing, Opts {optCmd = SetToken token} ) -> setToken token
        (Nothing, _) -> hPutStrLn stderr $ "You need to set token first with:\n" ++ name ++ " token TOKEN"

handle :: Opts -> LfxToken -> IO()
handle Opts { optCmd = On, optSelector = selector } token
    = changePower token selector "on"

handle Opts { optCmd = Off, optSelector = selector } token
    = changePower token selector "off"

handle Opts { optCmd = Toggle, optSelector = selector } token
    = void $ toggleLights token selector

handle Opts { optCmd = Ls, optSelector = selector } token = do
    Right lights <- listLights token selector
    mapM_ (putStrLn . formatLight) lights

handle Opts { optCmd = (Brightness percent), optSelector = selector } token
    = setBrightness token selector percent

handle Opts { optCmd = (SetToken newToken) } _oldToken
    = setToken newToken

handle Opts { optCmd = (IncrBrightness percent), optSelector = selector } token
    = changeBrightness token selector percent

handle Opts { optCmd = (DecrBrightness percent), optSelector = selector } token
    = changeBrightness token selector $ negate percent

handle Opts { optCmd = (Lifx.Command.Color newColor), optSelector = selector } token
    = setColor token selector newColor

data Opts =
    Opts { optSelector :: String
         , optCmd :: Cmd
         }

setToken :: LfxToken -> IO ()
setToken lfxtoken = do
    home <- getHomeDirectory
    BS.writeFile (home ++ "/.lfxtoken") lfxtoken
    putStrLn "LIFX token written to ~/.lfxtoken"

lifxToken :: IO (Maybe LfxToken)
lifxToken = do
      home <- getHomeDirectory
      token <- tryIOError $ BS.readFile $ home ++ "/.lfxtoken"
      return $ either (const Nothing) Just token

formatLight :: Light -> String
formatLight x =
    Lifx.id x ++ " :: " ++ label x ++ " :: " ++ Lifx.power x ++ " :: " ++ formatBrightness x

formatBrightness :: Light -> String
formatBrightness =
    (++"%") . show . round . (*100) . brightness
