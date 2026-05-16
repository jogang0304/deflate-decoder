module Src.HuffmanTree.Dynamic where

import Control.Monad (replicateM)
import Data.Maybe (fromMaybe)
import Src.Bits
import Src.HuffmanTree.General

-- декодирование динамических блоков (10)

codeLenOrder :: [Int]
codeLenOrder = [16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15]

decodeDynTrees :: BitParser (HuffmanTree, HuffmanTree)
decodeDynTrees = do
  hlit <- bits 5 -- количество кодов литералов/длин = HLIT + 257
  hdist <- bits 5 -- количество кодов дистанций = HDIST + 1
  hclen <- bits 4 -- количество кодов длин = HCLEN + 4

  -- читаем длины для дерева кодовых длин
  clLens <- replicateM (hclen + 4) (bits 3)
  let clPairs = zip codeLenOrder clLens
  let clAllPairs = [(sym, fromMaybe 0 (lookup sym clPairs)) | sym <- [0 .. 18]]
  let clTree = buildFromList $ assignCodes clAllPairs

  -- читаем длины для деревьев литералов и дистанций
  let total = hlit + 257 + hdist + 1
  lens <- decodeCodeLengths clTree total
  let litLens = take (hlit + 257) lens
  let distLens = drop (hlit + 257) lens

  -- читаем длины для деревьев литералов и дистанций
  let litTree = buildFromList $ assignCodes (zip [0 ..] litLens)
  let distTree = buildFromList $ assignCodes (zip [0 ..] distLens)

  return (litTree, distTree)

-- читаем список длин кодов (16/17/18 - RLE)
decodeCodeLengths :: HuffmanTree -> Int -> BitParser [Int]
decodeCodeLengths tree n = go n []
  where
    go 0 acc = return (reverse acc)
    go rem acc = do
      sym <- decodeSymbol tree
      case sym of
        16 -> do
          -- повторить предыдущую длину 3-6 раз
          extra <- bits 2
          let count = extra + 3
          let prev = if null acc then 0 else head acc
          go (rem - count) (replicate count prev ++ acc)
        17 -> do
          -- 3..10 нулей
          extra <- bits 3
          let count = extra + 3
          go (rem - count) (replicate count 0 ++ acc)
        18 -> do
          -- 11..138 нулей
          extra <- bits 7
          let count = extra + 11
          go (rem - count) (replicate count 0 ++ acc)
        l -> go (rem - 1) (l : acc)
