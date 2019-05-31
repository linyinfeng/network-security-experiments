module Main where

import           Rc4

import           Options.Applicative
import           Data.Semigroup                 ( (<>) )
import           Data.Array
import           Numeric                        ( showHex
                                                , readHex
                                                )
import           Data.Word
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Lazy.UTF8     as BLU
import           Control.Monad.State.Lazy
import           Debug.Trace

data CipherOption = CipherOption {
    keyString :: String,
    hexadecimalKey :: Bool
}

cipherOption :: Parser CipherOption
cipherOption =
    CipherOption
        <$> strOption
                (long "key" <> short 'k' <> help "RC4 key" <> metavar "STRING")
        <*> switch (long "hexadecimal-key" <> help "Hexadecimal key")

main :: IO ()
main = cipherMain =<< execParser opts
  where
    opts = info
        (cipherOption <**> helper)
        (fullDesc <> progDesc "RC4 Cipher" <> header "rc4 - Just RC4")

cipherMain :: CipherOption -> IO ()
cipherMain (CipherOption keyString hex) = do
    let keyList = if hex
            then convertFromHex keyString
            else BL.unpack $ BLU.fromString keyString
        keyArray = listArray (0, fromIntegral (length keyList) - 1) keyList
        rc4State = rc4 keyArray
        (fs, _)  = runState (mapM (const generateCipherF) $ repeat ()) rc4State
        f ws = [ f w | (f, w) <- zip fs ws ]
    contents <- BL.getContents
    BL.putStr $ BL.pack $ f $ BL.unpack contents

convertFromHex :: String -> [Word8]
convertFromHex []   = []
convertFromHex [d1] = error "invalid hex string"
convertFromHex (d1 : d2 : ds) =
    let (parsed, remain) = head $ readHex [d1, d2]
    in  case remain of
            [] -> parsed : convertFromHex ds
            _  -> error "hex string contains invalid characters"
