module Lifx.Command where

import  Data.ByteString   as BS (ByteString)

data Cmd
    = On
    | Off
    | Toggle
    | Ls
    | Brightness Integer
    | IncrBrightness Integer
    | DecrBrightness Integer
    | SetToken LfxToken

type LfxToken = BS.ByteString

