
{-# LANGUAGE RecursiveDo #-}

module ASM (
    ASM, ASMblage, asm, asm_result,
    byte, bytes, ascii, bytestring, binfile, fill, fillto, pad, hex, hexdata,
    le16, be16, le32, be32, le64, be64, lefloat, befloat, ledouble, bedouble,
    nothing, here,
    no_overflow,
    rep, repfor, skip, (>>.),
    allocate8, allocate16, allocate32, allocate64
) where

import Data.Word
import Data.Bits
import Data.Char
import Data.Monoid
import qualified Data.Sequence as S
import qualified Data.Foldable as F
import qualified Data.ByteString as B
import Text.Printf
import Assembler
import Unsafe.Coerce  -- for serializing floats and doubles
import System.IO.Unsafe  -- for binfile

type ASM ctr a = Assembler (S.Seq Word8) ctr a
type ASMblage ctr = Assemblage (S.Seq Word8) ctr

asm :: Num ctr => ASMblage ctr -> ASM ctr b -> (b, ASMblage ctr)
asm = assemble

asm_result :: ASMblage ctr -> B.ByteString
asm_result = B.pack . F.toList . assemblage_result

byte :: Num ctr => Word8 -> ASM ctr ()
byte = unit_assembler . S.singleton

bytes :: Num ctr => F.Foldable t => t Word8 -> ASM ctr ()
bytes bs = Assembler f where
    f (ann, pos) = (ann, F.foldl (const . (+ 1)) pos bs, S.fromList (F.toList bs), ())

ascii :: Num ctr => [Char] -> ASM ctr ()
ascii = bytes . map (fromIntegral . ord)

bytestring :: Num ctr => B.ByteString -> ASM ctr ()
bytestring bs = Assembler f where
    f (ann, pos) = (ann, pos + fromIntegral (B.length bs), S.fromList (B.unpack bs), ())

{-# NOINLINE binfile #-}
binfile :: String -> B.ByteString
binfile = unsafePerformIO . B.readFile

fill :: Integral ctr => ctr -> Word8 -> ASM ctr ()
fill size b = Assembler f where
    f (ann, pos) = if size >= 0
        then (ann, pos + size, S.replicate (fromIntegral size) b, ())
        else (ann, pos + size, err ann pos, ())
    err ann pos = error$ printf "Tried to fill a block with negative size%s at 0x%x (did something assemble too large?)"
                                (appendable_section_name ann) (toInteger pos)

fillto :: Integral ctr => ctr -> Word8 -> ASM ctr ()
fillto target b = Assembler f where
    f (ann, pos) = let
        payload = if target - pos < 0  -- allow target to be 0 on unsigned types for instance
            then error$ printf "fillto was called too late%s at 0x%x (did something assemble too large?)"
                               (appendable_section_name ann) (toInteger pos)
            else S.replicate (fromIntegral (target - pos)) b
        in (ann, target, payload, ())

pad :: Integral ctr => ctr -> Word8 -> ASM ctr a -> ASM ctr a
pad size = pad_assembler size . S.singleton

hex :: String -> [Word8]
hex [] = []
hex (c:rest) | not (isHexDigit c) = hex rest
hex (h:l:rest) | isHexDigit l = fromIntegral (digitToInt h * 16 + digitToInt l) : hex rest
hex _ = error "Odd number of hex digits in hexdata string."

hexdata :: Num ctr => String -> ASM ctr ()
hexdata = bytes . hex

le16 :: (Integral a, Integral ctr) => a -> ASM ctr ()
le16 x = Assembler f where
    w = fromIntegral x :: Word16
    res = S.fromList (map (fromIntegral . shiftR w . (8 *)) [0..1])
    f (ann, pos) = (ann, pos+2, res, ())
be16 :: (Integral a, Integral ctr) => a -> ASM ctr ()
be16 x = Assembler f where
    w = fromIntegral x :: Word16
    res = S.fromList (map (fromIntegral . shiftR w . (8 *)) (reverse [0..1]))
    f (ann, pos) = (ann, pos+2, res, ())
le32 :: (Integral a, Integral ctr) => a -> ASM ctr ()
le32 x = Assembler f where
    w = fromIntegral x :: Word32
    res = S.fromList (map (fromIntegral . shiftR w . (8 *)) [0..3])
    f (ann, pos) = (ann, pos+4, res, ())
be32 :: (Integral a, Integral ctr) => a -> ASM ctr ()
be32 x = Assembler f where
    w = fromIntegral x :: Word32
    res = S.fromList (map (fromIntegral . shiftR w . (8 *)) (reverse [0..3]))
    f (ann, pos) = (ann, pos+4, res, ())
le64 :: (Integral a, Integral ctr) => a -> ASM ctr ()
le64 x = Assembler f where
    w = fromIntegral x :: Word64
    res = S.fromList (map (fromIntegral . shiftR w . (8 *)) [0..7])
    f (ann, pos) = (ann, pos+8, res, ())
be64 :: (Integral a, Integral ctr) => a -> ASM ctr ()
be64 x = Assembler f where
    w = fromIntegral x :: Word64
    res = S.fromList (map (fromIntegral . shiftR w . (8 *)) (reverse [0..7]))
    f (ann, pos) = (ann, pos+8, res, ())
lefloat :: Integral ctr => Float -> ASM ctr ()
lefloat = le32 . (unsafeCoerce :: Float -> Word32)
befloat :: Integral ctr => Float -> ASM ctr ()
befloat = be32 . (unsafeCoerce :: Float -> Word32)
ledouble :: Integral ctr => Double -> ASM ctr ()
ledouble = le32 . (unsafeCoerce :: Double -> Word64)
bedouble :: Integral ctr => Double -> ASM ctr ()
bedouble = be32 . (unsafeCoerce :: Double -> Word64)

no_overflow' :: (Integral a, Integral b) => b -> b -> a -> Maybe b
no_overflow' min max x = if toInteger min <= toInteger x && toInteger x <= toInteger max
    then Just (fromIntegral x)
    else Nothing

no_overflow :: (Integral a, Integral b, Bounded b) => a -> Maybe b
no_overflow = no_overflow' minBound maxBound

rep :: Integral ctr => (ctr -> ASM ctr ()) -> ASM ctr a -> ASM ctr a
rep branch code = mdo
    start <- here
    res <- code
    branch start
    return res

repfor :: Integral ctr => ASM ctr () -> (ctr -> ASM ctr ()) -> ASM ctr a -> ASM ctr a
repfor init branch code = mdo
    init
    start <- here
    res <- code
    branch start
    return res

skip :: Integral ctr => (ctr -> ASM ctr ()) -> ASM ctr a -> ASM ctr a
skip branch code = mdo
    branch end
    res <- code
    end <- here
    return res

infixl 1 >>. 
cmp >>. branch = (cmp >>) . branch

allocate8 :: Integral siz => Word8 -> [siz] -> [Section Word8 ()]
allocate8 = allocate

allocate16 :: Integral siz => Word16 -> [siz] -> [Section Word16 ()]
allocate16 = allocate

allocate32 :: Integral siz => Word32 -> [siz] -> [Section Word32 ()]
allocate32 = allocate

allocate64 :: Integral siz => Word64 -> [siz] -> [Section Word64 ()]
allocate64 = allocate

