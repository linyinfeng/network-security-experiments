module Rc4Spec
  ( spec
  )
where

import           Test.Hspec
import           Rc4
import           Data.Word
import           Data.Array
import           Data.Char
import           Control.Monad.State.Lazy
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Lazy.UTF8     as BLU
import           Numeric                        ( showHex
                                                , readHex
                                                )

-- Currently no spce
spec :: Spec
spec = describe "Rc4" $ do
  it "sample 1 on wikipedia" $ do
    let fs        = cipherF "Key"
        plaintext = BL.unpack $ BLU.fromString "Plaintext"
        answer    = convertFromHex "BBF316E8D940AF0AD3"
    [ f w | (f, w) <- zip fs plaintext ] `shouldBe` answer
  it "sample 2 on wikipedia" $ do
    let fs        = cipherF "Wiki"
        plaintext = BL.unpack $ BLU.fromString "pedia"
        answer    = convertFromHex "1021BF0420"
    [ f w | (f, w) <- zip fs plaintext ] `shouldBe` answer
  it "sample 3 on wikipedia" $ do
    let fs        = cipherF "Secret"
        plaintext = BL.unpack $ BLU.fromString "Attack at dawn"
        answer    = convertFromHex "45A01F645FC35B383552544B9BF5"
    [ f w | (f, w) <- zip fs plaintext ] `shouldBe` answer

cipherF :: String -> [Word8 -> Word8]
cipherF key =
  let keyList  = map (fromIntegral . ord) key
      keyArray = listArray (0, fromIntegral (length keyList) - 1) keyList
      rc4State = rc4 keyArray
      (fs, _)  = runState (mapM (const generateCipherF) $ repeat ()) rc4State
  in  fs

pseudoRandom :: String -> [Word8]
pseudoRandom key =
  let
    keyList  = map (fromIntegral . ord) key
    keyArray = listArray (0, fromIntegral (length keyList) - 1) keyList
    rc4State = rc4 keyArray
    (ws, _)  = runState (mapM (const generatePseudoRandom) $ repeat ()) rc4State
  in
    ws

-- TODO: redundant in Main
convertFromHex :: String -> [Word8]
convertFromHex []   = []
convertFromHex [d1] = error "invalid hex string"
convertFromHex (d1 : d2 : ds) =
  let (parsed, remain) = head $ readHex [d1, d2]
  in  case remain of
        [] -> parsed : convertFromHex ds
        _  -> error "hex string contains invalid characters"
