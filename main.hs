module Main where

import Control.Applicative (Alternative (..), (<|>))
import Control.Monad.State (runStateT)
import Data.Word (Word8)
import Src.Base64
import Src.Bits
import Src.HuffmanTree.Dynamic
import Src.HuffmanTree.General
import Src.HuffmanTree.Raw
import Src.HuffmanTree.Static
import System.Environment (getArgs)
import System.Exit (exitFailure)

decodeBlocks :: [Word8] -> BitParser [Word8]
decodeBlocks buf = do
  bfinal <- bit -- последний ли блок
  btype <- bits 2

  out <- case btype of
    0 -> decodeStored buf
    1 -> decodeLZ fixedLitLenTree fixedDistTree buf
    2 -> do
      (litTree, distTree) <- decodeDynTrees
      decodeLZ litTree distTree buf
    _ -> empty -- неизвестный тип блока
  if bfinal == 1
    then return out
    else decodeBlocks out

-- base64 → байты → deflate → строка
decompressBase64 :: String -> Maybe String
decompressBase64 b64 = do
  bytes <- decodeBase64 b64
  let initState = BitState bytes 0
  (result, _) <- runStateT (decodeBlocks []) initState
  return $ map (toEnum . fromIntegral) result

main :: IO ()
main = do
  args <- getArgs
  case args of
    [b64] -> case decompressBase64 b64 of
      Nothing -> do
        putStrLn "Error: failed to decompress"
        exitFailure
      Just out -> putStr out
    _ -> do
      putStrLn "Usage: decompress <base64>"
      exitFailure
