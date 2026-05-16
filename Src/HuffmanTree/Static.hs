module Src.HuffmanTree.Static where

import Data.Word (Word8)
import Src.Bits
import Src.HuffmanTree.General

-- статическое дерево Хаффмана (01)

-- Длины кодов для литералов/длин (0...287)
fixedLitLenLengths :: [Int]
fixedLitLenLengths =
  replicate 144 8 -- 0..143:   8 бит
    ++ replicate 112 9 -- 144..255: 9 бит
    ++ replicate 24 7 -- 256..279: 7 бит
    ++ replicate 8 8 -- 280..287: 8 бит

-- Длины кодов для дистанций (0...29)
fixedDistLengths :: [Int]
fixedDistLengths = replicate 30 5

fixedLitLenTree :: HuffmanTree
fixedLitLenTree =
  buildFromList $ assignCodes (zip [0 ..] fixedLitLenLengths)

fixedDistTree :: HuffmanTree
fixedDistTree =
  buildFromList $ assignCodes (zip [0 ..] fixedDistLengths)

-- (базовое значение, дополнительные биты) для кодов длин 257..285
-- комментариями типа скобочки раставил
lengthTable :: [(Int, Int)]
lengthTable =
  [ (3, 0), -- .\
    (4, 0), -- .|
    (5, 0), -- .|> 257..261
    (6, 0), -- .|
    (7, 0), -- ./
    (8, 0), --  .\
    (9, 0), --  .|> 262..264
    (10, 0), -- ./
    (11, 1), -- .\
    (13, 1), -- .|
    (15, 1), -- .|>265..268
    (17, 1), -- ./
    (19, 2), -- .\
    (23, 2), -- .|
    (27, 2), -- .|>269..272
    (31, 2), -- ./
    (35, 3), -- .\
    (43, 3), -- .|
    (51, 3), -- .|> 273..276
    (59, 3), -- ./
    (67, 4), --  .\
    (83, 4), --  .|
    (99, 4), --  .|> 277..280
    (115, 4), -- ./
    (131, 5), -- .\
    (163, 5), -- .|
    (195, 5), -- .|> 281..284
    (227, 5), -- ./
    (258, 0) -- 285
  ]

-- (базовое значение, дополнительные биты) для кодов дистанций 0..29
distTable :: [(Int, Int)]
distTable =
  [ (1, 0),
    (2, 0),
    (3, 0),
    (4, 0),
    (5, 1),
    (7, 1),
    (9, 2),
    (13, 2),
    (17, 3),
    (25, 3),
    (33, 4),
    (49, 4),
    (65, 5),
    (97, 5),
    (129, 6),
    (193, 6),
    (257, 7),
    (385, 7),
    (513, 8),
    (769, 8),
    (1025, 9),
    (1537, 9),
    (2049, 10),
    (3073, 10),
    (4097, 11),
    (6145, 11),
    (8193, 12),
    (12289, 12),
    (16385, 13),
    (24577, 13)
  ]

-- Декодируем серию символов/длин/дистанций и пишем в выходной буфер
decodeLZ :: HuffmanTree -> HuffmanTree -> [Word8] -> BitParser [Word8]
decodeLZ litTree distTree buf = do
  sym <- decodeSymbol litTree
  case sym of
    256 -> return buf -- конец блока
    s | s < 256 -> decodeLZ litTree distTree (buf ++ [fromIntegral s])
    s -> do
      -- s в 257..285: код длины
      let idx = s - 257
      let (base, eb) = lengthTable !! idx
      extra <- bits eb
      let len = base + extra
      -- код дистанции
      distCode <- decodeSymbol distTree
      let (dbase, db) = distTable !! distCode
      dextra <- bits db
      let dist = dbase + dextra
      -- копируем из буфера
      let bufLen = length buf
      let startIdx = bufLen - dist
      let copied = copyBytes buf startIdx len
      decodeLZ litTree distTree (buf ++ copied)

copyBytes :: [Word8] -> Int -> Int -> [Word8]
copyBytes buf start len = take len $ map (\i -> buf !! ((start + i) `mod` length buf)) [0 ..]
