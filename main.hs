module Main where

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

-- https://datatracker.ietf.org/doc/html/rfc4648#section-4
base64Chars :: String
base64Chars = ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9'] ++ "+/"

base64CharToInt :: Char -> Maybe Int
base64CharToInt c = elemIndex c base64Chars

-- Декодируем строку Base64 в список байт
decodeBase64 :: String -> Maybe [Word8]
decodeBase64 s = do
  let s' = filter (/= '\n') s -- убираем переносы строк
  groups <- splitGroups4 s'
  concat <$> mapM decodeGroup groups
  where
    splitGroups4 [] = Just []
    splitGroups4 xs
      | length xs < 4 = Nothing
      | otherwise = do
          rest <- splitGroups4 (drop 4 xs)
          return (take 4 xs : rest)

    decodeGroup [a, b, c, d]
      | c == '=' && d == '=' = do
          va <- base64CharToInt a
          vb <- base64CharToInt b
          return [fromIntegral ((va `shiftL` 2) .|. (vb `shiftR` 4))]
      | d == '=' = do
          va <- base64CharToInt a
          vb <- base64CharToInt b
          vc <- base64CharToInt c
          let n = (va `shiftL` 10) .|. (vb `shiftL` 4) .|. (vc `shiftR` 2)
          return
            [ fromIntegral (n `shiftR` 8),
              fromIntegral (n .&. 0xFF)
            ]
      | otherwise = do
          va <- base64CharToInt a
          vb <- base64CharToInt b
          vc <- base64CharToInt c
          vd <- base64CharToInt d
          let n = (va `shiftL` 18) .|. (vb `shiftL` 12) .|. (vc `shiftL` 6) .|. vd
          return
            [ fromIntegral (n `shiftR` 16),
              fromIntegral ((n `shiftR` 8) .&. 0xFF),
              fromIntegral (n .&. 0xFF)
            ]
    decodeGroup _ = Nothing

-- Дерево Хаффмана
data HuffmanTree
  = HLeaf Int -- значение
  | HNode HuffmanTree HuffmanTree -- 0 — левый, 1 — правый
  deriving (Show)

-- строим дерево сразу из списка (код, длина, символ)
buildFromList :: [(Int, Int, Int)] -> HuffmanTree
buildFromList = foldl' insertTriple emptyNode
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

---------------------------------------
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

-- Присваиваем коды по длинам
assignCodes :: [(Int, Int)] -> [(Int, Int, Int)]
assignCodes pairs =
  let usable = filter (\(_, l) -> l > 0) pairs
      maxLen = if null usable then 0 else maximum (map snd usable)
      blCount l = length $ filter (\(_, len) -> len == l) usable
      initNext =
        snd $
          foldl'
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
      (_, coded) = foldl' step (nextRef, []) (sortBy (comparing snd) usable)
   in coded

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

---------------------------------------
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

---------------------------------------
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

---------------------------------------
-- основной цикл

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
