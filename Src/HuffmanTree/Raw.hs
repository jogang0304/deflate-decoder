module Src.HuffmanTree.Raw where

import Data.Bits (shiftL, (.|.))
import Data.Word (Word8)
import Src.Bits

-- блок без сжатия (00)
decodeStored :: [Word8] -> BitParser [Word8]
decodeStored buf = do
  alignByte
  lenLo <- rawByte
  lenHi <- rawByte
  _nlenLo <- rawByte
  _nlenHi <- rawByte
  let len = fromIntegral lenLo .|. (fromIntegral lenHi `shiftL` 8)
  bs <- rawBytes len
  return (buf ++ bs)
