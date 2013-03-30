
{-# LANGUAGE RecursiveDo #-}

module ASM (
    ASM,
    byte, bytes, ascii, bytestring, binfile, fill, fillto, pad, hex, hexdata,
    le16, be16, le32, be32, le64, be64, lefloat, befloat, ledouble, bedouble,
    nothing, here,
    asm, asm_result, no_overflow,
    startof, endof, startend, sizeof,
    rep, repfor, skip, (>>.),
    Allocation(..), provide, provide_at,
    allocate, allocate8, allocate16, allocate32, allocate64
) where

import Data.Word
import Data.Bits
import Data.Char
import Data.Monoid
import qualified Data.Sequence as S
import qualified Data.Foldable as F
import qualified Data.ByteString as B
import Assembly
import Unsafe.Coerce  -- for serializing floats and doubles
import System.IO.Unsafe  -- for binfile

type ASM ctr a = Assembler (S.Seq Word8) ctr a

asm :: Num ctr => Assembly (S.Seq Word8) a ctr -> ASM ctr a -> Assembly (S.Seq Word8) a ctr
asm = assemble

asm_result :: Assembly (S.Seq Word8) a ctr -> B.ByteString
asm_result = B.pack . F.toList . assembly_result

byte :: Num ctr => Word8 -> ASM ctr ()
byte = unit_assembler . S.singleton

bytes :: Num ctr => F.Foldable t => t Word8 -> ASM ctr ()
bytes bs = Assembler f where
    f pos = (F.foldl (const . (+ 1)) pos bs, S.fromList (F.toList bs), ())

ascii :: Num ctr => [Char] -> ASM ctr ()
ascii = bytes . map (fromIntegral . ord)

bytestring :: Num ctr => B.ByteString -> ASM ctr ()
bytestring bs = Assembler f where
    f pos = (pos + fromIntegral (B.length bs), S.fromList (B.unpack bs), ())

{-# NOINLINE binfile #-}
binfile :: String -> B.ByteString
binfile = unsafePerformIO . B.readFile

fill :: Integral ctr => ctr -> Word8 -> ASM ctr ()
fill size b = if size >= 0
    then Assembler (\pos -> (pos + size, S.replicate (fromIntegral size) b, ()))
    else error$ "Tried to fill a block with negative size (did something assemble too large?)"

fillto :: Integral ctr => ctr -> Word8 -> ASM ctr ()
fillto target b = Assembler f where
    f pos = let
        payload = if target - pos < 0  -- allow target to be 0 on unsigned types for instance
            then error$ "fillto was called too late (did something assemble too large?)"
            else S.replicate (fromIntegral (target - pos)) b
        in (target, payload, ())

pad :: Integral ctr => ctr -> Word8 -> ASM ctr a -> ASM ctr a
pad size = pad_assembler size . S.singleton

hex :: String -> [Word8]
hex [] = []
hex (c:rest) | not (isHexDigit c) = hex rest
hex (h:l:rest) | isHexDigit l = fromIntegral (digitToInt h * 16 + digitToInt l) : hex rest
hex _ = error "Odd number of hex digits in hexdata string."

hexdata :: Num ctr => String -> ASM ctr ()
hexdata = bytes . hex

le16 :: Integral ctr => Word16 -> ASM ctr ()
le16 w = do
    byte$ fromIntegral w
    byte$ fromIntegral (shiftR w 8)
be16 :: Integral ctr => Word16 -> ASM ctr ()
be16 w = do
    byte$ fromIntegral (shiftR w 8)
    byte$ fromIntegral w
le32 :: Integral ctr => Word32 -> ASM ctr ()
le32 w = do
    byte$ fromIntegral w
    byte$ fromIntegral (shiftR w 8)
    byte$ fromIntegral (shiftR w 16)
    byte$ fromIntegral (shiftR w 24)
be32 :: Integral ctr => Word32 -> ASM ctr ()
be32 w = do
    byte$ fromIntegral (shiftR w 24)
    byte$ fromIntegral (shiftR w 16)
    byte$ fromIntegral (shiftR w 8)
    byte$ fromIntegral w
le64 :: Integral ctr => Word64 -> ASM ctr ()
le64 w = do
    byte$ fromIntegral w
    byte$ fromIntegral (shiftR w 8)
    byte$ fromIntegral (shiftR w 16)
    byte$ fromIntegral (shiftR w 24)
    byte$ fromIntegral (shiftR w 32)
    byte$ fromIntegral (shiftR w 40)
    byte$ fromIntegral (shiftR w 48)
    byte$ fromIntegral (shiftR w 56)
be64 :: Integral ctr => Word64 -> ASM ctr ()
be64 w = do
    byte$ fromIntegral (shiftR w 56)
    byte$ fromIntegral (shiftR w 48)
    byte$ fromIntegral (shiftR w 40)
    byte$ fromIntegral (shiftR w 32)
    byte$ fromIntegral (shiftR w 24)
    byte$ fromIntegral (shiftR w 16)
    byte$ fromIntegral (shiftR w 8)
    byte$ fromIntegral w
lefloat :: Integral ctr => Float -> ASM ctr ()
lefloat = le32 . unsafeCoerce
befloat :: Integral ctr => Float -> ASM ctr ()
befloat = be32 . unsafeCoerce
ledouble :: Integral ctr => Double -> ASM ctr ()
ledouble = le32 . unsafeCoerce
bedouble :: Integral ctr => Double -> ASM ctr ()
bedouble = be32 . unsafeCoerce


no_overflow' :: (Integral a, Integral b) => b -> b -> a -> Maybe b
no_overflow' min max x = let
    in if toInteger min <= toInteger x && toInteger x <= toInteger max
        then Just (fromIntegral x)
        else Nothing

no_overflow :: (Integral a, Integral b, Bounded b) => a -> Maybe b
no_overflow = no_overflow' minBound maxBound


startof x = do
    start <- here
    x
    return start

endof x = x >> here

startend x = do
    start <- here
    x
    end <- here
    return (start, end)

sizeof x = do
    start <- here
    x
    end <- here
    return (end - start)

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


data Allocation a = Allocation a a deriving (Show, Eq, Ord)
allocation_start (Allocation x _) = x
allocation_size (Allocation _ x) = x
allocation_end (Allocation start size) = start + size
instance HasArea Allocation where
    start = allocation_start
    size = allocation_size
    end = allocation_end

allocate :: (Num a, Integral b) => a -> [b] -> [Allocation a]
allocate start [] = []
allocate start (size:sizes) = Allocation start isize : allocate (start + isize) sizes where
    isize = fromIntegral size

type Allocation8 = Allocation Word8
allocate8 :: Integral a => Word8 -> [a] -> [Allocation8]
allocate8 = allocate
type Allocation16 = Allocation Word16
allocate16 :: Integral a => Word16 -> [a] -> [Allocation16]
allocate16 = allocate
type Allocation32 = Allocation Word32
allocate32 :: Integral a => Word32 -> [a] -> [Allocation32]
allocate32 = allocate
type Allocation64 = Allocation Word64
allocate64 :: Integral a => Word64 -> [a] -> [Allocation64]
allocate64 = allocate

provide :: Integral ctr => Allocation ctr -> ASM ctr a -> ASM ctr a
provide res code = mdo
    enforce_counter (start res)
    ret <- code
    enforce_counter (end res)
    return ret

provide_at :: Integral ctr => ctr -> Allocation ctr -> ASM ctr a -> ASM ctr a
provide_at off res code = mdo
    enforce_counter (start res + off)
    ret <- code
    enforce_counter (end res + off)
    return ret

instance (Num a, Eq a) => Monoid (Allocation a) where
    mempty = Allocation 0 0
    mappend a b = if end a == start b
        then Allocation (start a) (size a + size b)
        else error$ "Tried to merge resources that didn't match."

instance Num a => Num (Allocation a) where
    (+) = error$ "Can't (+) Allocation."
    (*) = error$ "Can't (*) Allocation."
    (-) = error$ "Can't (-) Allocation."
    abs = error$ "Can't abs Allocation."
    signum = error$ "Can't signum Allocation."
    fromInteger x = Allocation (fromInteger x) 0

instance Enum a => Enum (Allocation a) where
    succ (Allocation st sz) = Allocation (succ st) sz
    pred (Allocation st sz) = Allocation (pred st) sz
    toEnum = error$ "Can't toEnum to get Allocation."
    fromEnum = error$ "Can't fromEnum Allocation (you can toInteger it though."

instance Real a => Real (Allocation a) where
    toRational = toRational . start

instance Integral a => Integral (Allocation a) where
    quotRem = error$ "Can't quotRem Allocation.  It's only Integral for its toInteger."
    toInteger (Allocation s _) = toInteger s

 -- Kinda icky but okay
instance Bounded a => Bounded (Allocation a) where
    minBound = Allocation minBound minBound
    maxBound = Allocation maxBound maxBound

