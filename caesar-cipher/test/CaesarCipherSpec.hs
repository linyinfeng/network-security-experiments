
module CaesarCipherSpec (spec) where

import Test.Hspec
import CaesarCipher

spec :: Spec
spec = do
  describe "CaesarCipher" $ do
    it "shift 3" $ do
      cipherString 3 "Hello, world!" `shouldBe` "Khoor, zruog!"
    it "shift -3" $ do
      cipherString (-3) "Khoor, zruog!" `shouldBe` "Hello, world!"
    it "all alpha shift 3" $ do
      cipherString 3 "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" `shouldBe` "defghijklmnopqrstuvwxyzabcDEFGHIJKLMNOPQRSTUVWXYZABC"
    it "all alpha shift -3" $ do
      cipherString (-3) "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" `shouldBe` "xyzabcdefghijklmnopqrstuvwXYZABCDEFGHIJKLMNOPQRSTUVW"