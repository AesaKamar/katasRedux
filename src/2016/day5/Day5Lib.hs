{-# LANGUAGE OverloadedStrings #-}
module Day5Lib where

import Crypto.Hash (digestToHexByteString, digestFromByteString )
import qualified Crypto.Hash.MD5 as MD5


input = "ugkcyxxp"


sss = digestToHexByteString $ MD5.hash "hello"
