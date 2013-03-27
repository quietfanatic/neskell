
module ASM (
    ASM,
    byte, bytes, ascii, fill, hexdata,
    le16, be16, le32, be32, le64, be64, lefloat, befloat, ledouble, bedouble,
    nothing, here, set_counter,
    assemble_asm, no_overflow
) where

import Data.Word
import Data.Bits
import Data.Char
import qualified Data.Sequence as S
import qualified Data.Foldable as F
import qualified Data.ByteString as B
import Assembly
import Unsafe.Coerce

type ASM a = Assembly (S.Seq Word8) Int a

assemble_asm :: ASM a -> B.ByteString
assemble_asm = B.pack . F.toList . assemble

byte :: Word8 -> ASM ()
byte = unit . S.singleton

bytes :: F.Foldable t => t Word8 -> ASM ()
bytes bs = Assembly (\c -> (S.fromList (F.toList bs), F.foldl (const . succ) c bs, ()))

ascii :: [Char] -> ASM ()
ascii = bytes . map (fromIntegral . ord)

fill :: Int -> Word8 -> ASM ()
fill size b = if size >= 0
    then Assembly (\c -> (S.replicate size b, c + size, ()))
    else error$ "Tried to fill a block with negative size (did something assemble too large?)"

hexdata :: String -> ASM ()
hexdata = bytes . f where
    f [] = []
    f (c:rest) | not (isHexDigit c) = f rest
    f (h:l:rest) | isHexDigit l = fromIntegral (digitToInt h * 16 + digitToInt l) : f rest
    f _ = error "Odd number of hex digits in hexdata string."

le16 :: Word16 -> ASM ()
le16 w = do
    byte$ fromIntegral w
    byte$ fromIntegral (shiftR w 8)
be16 :: Word16 -> ASM ()
be16 w = do
    byte$ fromIntegral (shiftR w 8)
    byte$ fromIntegral w
le32 :: Word32 -> ASM ()
le32 w = do
    byte$ fromIntegral w
    byte$ fromIntegral (shiftR w 8)
    byte$ fromIntegral (shiftR w 16)
    byte$ fromIntegral (shiftR w 24)
be32 :: Word32 -> ASM ()
be32 w = do
    byte$ fromIntegral (shiftR w 24)
    byte$ fromIntegral (shiftR w 16)
    byte$ fromIntegral (shiftR w 8)
    byte$ fromIntegral w
le64 :: Word64 -> ASM ()
le64 w = do
    byte$ fromIntegral w
    byte$ fromIntegral (shiftR w 8)
    byte$ fromIntegral (shiftR w 16)
    byte$ fromIntegral (shiftR w 24)
    byte$ fromIntegral (shiftR w 32)
    byte$ fromIntegral (shiftR w 40)
    byte$ fromIntegral (shiftR w 48)
    byte$ fromIntegral (shiftR w 56)
be64 :: Word64 -> ASM ()
be64 w = do
    byte$ fromIntegral (shiftR w 56)
    byte$ fromIntegral (shiftR w 48)
    byte$ fromIntegral (shiftR w 40)
    byte$ fromIntegral (shiftR w 32)
    byte$ fromIntegral (shiftR w 24)
    byte$ fromIntegral (shiftR w 16)
    byte$ fromIntegral (shiftR w 8)
    byte$ fromIntegral w
lefloat :: Float -> ASM ()
lefloat = le32 . unsafeCoerce
befloat :: Float -> ASM ()
befloat = be32 . unsafeCoerce
ledouble :: Double -> ASM ()
ledouble = le32 . unsafeCoerce
bedouble :: Double -> ASM ()
bedouble = be32 . unsafeCoerce


no_overflow' :: (Integral a, Integral b) => b -> b -> a -> Maybe b
no_overflow' min max x = let
    in if toInteger min <= toInteger x && toInteger x <= toInteger max
        then Just (fromIntegral x)
        else Nothing

no_overflow :: (Integral a, Integral b, Bounded b) => a -> Maybe b
no_overflow = no_overflow' minBound maxBound
