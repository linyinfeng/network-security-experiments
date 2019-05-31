module Rc4
    ( Rc4
    , rc4
    , generateCipherF
    , generatePseudoRandom
    )
where

import           Data.Array
import           Data.Word
import           Data.Bits
import           Control.Monad.State.Lazy

data Rc4 = Rc4 {
    stateArray :: Array Word8 Word8,
    i :: Word8,
    j :: Word8
}
type Key = Array Word8 Word8
type Rc4Array = Array Word8 Word8

instance Show Rc4 where
    show (Rc4 a i j) =
        "RC4 " ++ show a ++ " i = " ++ show i ++ " j = " ++ show j

rc4Min, rc4Max :: Word8
rc4Min = minBound
rc4Max = maxBound

swap :: Word8 -> Word8 -> State Rc4Array ()
swap i j = do
    s <- get
    let ei = s ! i
        ej = s ! j
    put (s // [(i, ej), (j, ei)])

initialArray :: Rc4Array
initialArray = array (rc4Min, rc4Max) [ (i, i) | i <- [rc4Min .. rc4Max] ]

keyScheduling :: Key -> Rc4Array
keyScheduling key = fst $ execState
    (mapM_ (keySchedulingStep key) [rc4Min .. rc4Max])
    (initialArray, 0)

keySchedulingStep :: Key -> Word8 -> State (Rc4Array, Word8) ()
keySchedulingStep key i = do
    (s, j) <- get
    let keyLength = fromIntegral $ rangeSize $ bounds key
        newJ      = j + (s ! i) + key ! mod i keyLength
        newA      = execState (swap i newJ) s
    put (newA, newJ)

generatePseudoRandom :: State Rc4 Word8
generatePseudoRandom = do
    (Rc4 s i j) <- get
    let newI = i + 1
        newJ = j + s ! newI
        newS = execState (swap newI newJ) s
    put (Rc4 newS newI newJ)
    return $ newS ! (newS ! newI + newS ! newJ)

generateCipherF :: State Rc4 (Word8 -> Word8)
generateCipherF = xor <$> generatePseudoRandom

rc4 :: Key -> Rc4
rc4 key = Rc4 (keyScheduling key) 0 0
