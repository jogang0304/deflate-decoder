module Src.HuffmanTree.General where

import Data.Bits (shiftL, testBit)
import Data.List (foldl, sortBy)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Src.Bits

-- Дерево Хаффмана
data HuffmanTree
  = HLeaf Int -- значение
  | HNode HuffmanTree HuffmanTree -- 0 — левый, 1 — правый
  deriving (Show)

-- строим дерево сразу из списка (код, длина, символ)
buildFromList :: [(Int, Int, Int)] -> HuffmanTree
buildFromList = foldl insertTriple emptyNode
  where
    emptyNode = HNode emptyLeaf emptyLeaf
    emptyLeaf = HLeaf (-1)
    insertTriple tree (sym, len, code) = go tree (len - 1)
      where
        go _ (-1) = HLeaf sym
        go (HLeaf _) depth
          | testBit code depth = HNode emptyLeaf (go emptyLeaf (depth - 1))
          | otherwise = HNode (go emptyLeaf (depth - 1)) emptyLeaf
        go (HNode l r) depth
          | testBit code depth = HNode l (go r (depth - 1))
          | otherwise = HNode (go l (depth - 1)) r

decodeSymbol :: HuffmanTree -> BitParser Int
decodeSymbol (HLeaf v) = return v
decodeSymbol (HNode l r) = do
  b <- bit
  if b == 0 then decodeSymbol l else decodeSymbol r

-- Присваиваем коды по длинам
assignCodes :: [(Int, Int)] -> [(Int, Int, Int)]
assignCodes pairs =
  let usable = filter (\(_, l) -> l > 0) pairs
      maxLen = if null usable then 0 else maximum (map snd usable)
      blCount l = length $ filter (\(_, len) -> len == l) usable
      initNext =
        snd $
          foldl
            ( \(c, acc) l ->
                let c' = (c + blCount (l - 1)) `shiftL` 1
                 in (c', acc ++ [(l, c')])
            )
            (0, [])
            [1 .. maxLen]
      nextRef = initNext
      step (nxt, acc) (sym, len) =
        let c = fromMaybe 0 (lookup len nxt)
            nxt' = map (\(l, v) -> if l == len then (l, v + 1) else (l, v)) nxt
         in (nxt', (sym, len, c) : acc)
      (_, coded) = foldl step (nextRef, []) (sortBy (comparing snd) usable)
   in coded
