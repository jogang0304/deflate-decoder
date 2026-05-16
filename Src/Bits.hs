module Src.Bits where

import Control.Applicative (Alternative (..), (<|>))
import Control.Monad (forM_, replicateM, unless, when)
import Control.Monad.State (StateT (..), get, gets, lift, modify, put, runStateT)
import Data.Bits (shiftL, shiftR, testBit, xor, (.&.), (.|.))
import Data.Char (digitToInt, intToDigit, isAlphaNum, ord)
import Data.List (elemIndex, foldl', isPrefixOf, sortBy)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ord (comparing)
import Data.Word (Word8)
import System.Environment (getArgs)
import System.Exit (exitFailure)

-- Состояние = список байтов + позиция внутри текущего байта (0..7)
data BitState = BitState
  { bsBytes :: [Word8], -- оставшиеся байты
    bsBitPos :: Int -- следующий бит внутри головного байта (0 = LSB)
  }
  deriving (Show)

type BitParser a = StateT BitState Maybe a

-- чтение 1 бита
bit :: BitParser Int
bit = StateT $ \bs -> case bsBytes bs of
  [] -> Nothing
  (b : rest) ->
    let pos = bsBitPos bs
        val = if testBit b pos then 1 else 0
        bs' =
          if pos == 7
            then BitState rest 0
            else BitState (b : rest) (pos + 1)
     in Just (val, bs')

-- чтение n бит, LSB first
bits :: Int -> BitParser Int
bits 0 = pure 0
bits n = do
  b <- bit
  bs <- bits (n - 1)
  return $ b .|. (bs `shiftL` 1)

-- чтение n бит, MSB first
bitsMSB :: Int -> BitParser Int
bitsMSB 0 = pure 0
bitsMSB n = do
  b <- bit
  bs <- bitsMSB (n - 1)
  return $ (b `shiftL` (n - 1)) .|. bs

-- Выровняться по границе байта (пропустить оставшиеся биты текущего байта)
alignByte :: BitParser ()
alignByte = StateT $ \bs ->
  case bsBitPos bs of
    0 -> Just ((), bs)
    _ -> Just ((), bs {bsBytes = tail (bsBytes bs), bsBitPos = 0})

-- чтение одного байта (после выравнивания)
rawByte :: BitParser Word8
rawByte = StateT $ \bs -> case bsBytes bs of
  [] -> Nothing
  (b : rest) -> Just (b, bs {bsBytes = rest})

-- чтение n байт подряд
rawBytes :: Int -> BitParser [Word8]
rawBytes n = replicateM n rawByte
