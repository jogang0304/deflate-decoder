module Src.Base64 where

import Data.Bits (shiftL, shiftR, testBit, xor, (.&.), (.|.))
import Data.List (elemIndex)
import Data.Word (Word8)

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
