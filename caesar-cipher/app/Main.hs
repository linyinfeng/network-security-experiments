module Main where

import CaesarCipher

import Options.Applicative
import Data.Semigroup ((<>))

data CipherOption = CipherOption {
    shift :: Int,
    decrypt :: Bool
}

cipherOption :: Parser CipherOption
cipherOption = CipherOption 
    <$> option auto (
        long "shift" <>
        short 's' <>
        help "Shift steps" <>
        metavar "NUMBER"
    )
    <*> switch (
        long "decrypt" <>
        short 'd' <>
        help "Decrypt with shift number"
    )

main :: IO ()
main = cipherMain =<< execParser opts
  where
    opts = info (cipherOption <**> helper)
        (fullDesc <>
        progDesc "Print a greeting for TARGET" <>
        header "caesar-chiper - Just a caesar-cipher")

cipherMain :: CipherOption -> IO ()
cipherMain (CipherOption s d) = do
    contents <- getContents
    putStr (cipher contents)
    where cipher = cipherString (if d then -s else s)
