
{-# LANGUAGE RecursiveDo #-}

module ASM (
    ASM,
    byte, bytes, ascii, bytestring, binfile, fill, pad, hex, hexdata,
    le16, be16, le32, be32, le64, be64, lefloat, befloat, ledouble, bedouble,
    nothing, here, set_counter,
    assemble_asm, asm, no_overflow,
    startof, endof, sizeof,
    rep, repfor, skip, (>>.),
    Res, start, size, end, resources, provide, provide_at, merge_res
) where

import Data.Word
import Data.Bits
import Data.Char
import qualified Data.Sequence as S
import qualified Data.Foldable as F
import qualified Data.ByteString as B
import Assembly
import Unsafe.Coerce  -- for serializing floats and doubles
import System.IO.Unsafe  -- for binfile

type ASM ctr a = Assembly (S.Seq Word8) ctr a

assemble_asm :: Num ctr => ASM ctr a -> B.ByteString
assemble_asm = B.pack . F.toList . assemble

asm :: Num ctr => ctr -> ASM ctr a -> (B.ByteString, ctr, a)
asm start (Assembly f) = let
    (seq, finish, ret) = f start
    in (B.pack (F.toList seq), finish, ret)

byte :: Num ctr => Word8 -> ASM ctr ()
byte = unit . S.singleton

bytes :: Num ctr => F.Foldable t => t Word8 -> ASM ctr ()
bytes bs = Assembly (\c -> (S.fromList (F.toList bs), F.foldl (const . (+ 1)) c bs, ()))

ascii :: Num ctr => [Char] -> ASM ctr ()
ascii = bytes . map (fromIntegral . ord)

bytestring :: Num ctr => B.ByteString -> ASM ctr ()
bytestring bs = Assembly (\c -> (S.fromList (B.unpack bs), fromIntegral (B.length bs), ()))

{-# NOINLINE binfile #-}
binfile :: String -> B.ByteString
binfile = unsafePerformIO . B.readFile

fill :: Integral ctr => ctr -> Word8 -> ASM ctr ()
fill size b = if size >= 0
    then Assembly (\c -> (S.replicate (fromIntegral size) b, c + size, ()))
    else error$ "Tried to fill a block with negative size (did something assemble too large?)"

pad :: Integral ctr => ctr -> Word8 -> ASM ctr a -> ASM ctr a
pad size = pad_assembly size . S.singleton

hex :: String -> [Word8]
hex [] = []
hex (c:rest) | not (isHexDigit c) = hex rest
hex (h:l:rest) | isHexDigit l = fromIntegral (digitToInt h * 16 + digitToInt l) : hex rest
hex _ = error "Odd number of hex digits in hexdata string."

hexdata :: Num ctr => String -> ASM ctr ()
hexdata = bytes . hex

le16 :: (Num ctr, Show ctr) => Word16 -> ASM ctr ()
le16 w = do
    byte$ fromIntegral w
    byte$ fromIntegral (shiftR w 8)
be16 :: (Num ctr, Show ctr) => Word16 -> ASM ctr ()
be16 w = do
    byte$ fromIntegral (shiftR w 8)
    byte$ fromIntegral w
le32 :: (Num ctr, Show ctr) => Word32 -> ASM ctr ()
le32 w = do
    byte$ fromIntegral w
    byte$ fromIntegral (shiftR w 8)
    byte$ fromIntegral (shiftR w 16)
    byte$ fromIntegral (shiftR w 24)
be32 :: (Num ctr, Show ctr) => Word32 -> ASM ctr ()
be32 w = do
    byte$ fromIntegral (shiftR w 24)
    byte$ fromIntegral (shiftR w 16)
    byte$ fromIntegral (shiftR w 8)
    byte$ fromIntegral w
le64 :: (Num ctr, Show ctr) => Word64 -> ASM ctr ()
le64 w = do
    byte$ fromIntegral w
    byte$ fromIntegral (shiftR w 8)
    byte$ fromIntegral (shiftR w 16)
    byte$ fromIntegral (shiftR w 24)
    byte$ fromIntegral (shiftR w 32)
    byte$ fromIntegral (shiftR w 40)
    byte$ fromIntegral (shiftR w 48)
    byte$ fromIntegral (shiftR w 56)
be64 :: (Num ctr, Show ctr) => Word64 -> ASM ctr ()
be64 w = do
    byte$ fromIntegral (shiftR w 56)
    byte$ fromIntegral (shiftR w 48)
    byte$ fromIntegral (shiftR w 40)
    byte$ fromIntegral (shiftR w 32)
    byte$ fromIntegral (shiftR w 24)
    byte$ fromIntegral (shiftR w 16)
    byte$ fromIntegral (shiftR w 8)
    byte$ fromIntegral w
lefloat :: (Num ctr, Show ctr) => Float -> ASM ctr ()
lefloat = le32 . unsafeCoerce
befloat :: (Num ctr, Show ctr) => Float -> ASM ctr ()
befloat = be32 . unsafeCoerce
ledouble :: (Num ctr, Show ctr) => Double -> ASM ctr ()
ledouble = le32 . unsafeCoerce
bedouble :: (Num ctr, Show ctr) => Double -> ASM ctr ()
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

sizeof x = do
    start <- here
    x
    end <- here
    return (end - start)

rep :: Show ctr => (ctr -> ASM ctr ()) -> ASM ctr a -> ASM ctr a
rep branch code = mdo
    start <- here
    res <- code
    branch start
    return res

repfor :: Show ctr => ASM ctr () -> (ctr -> ASM ctr ()) -> ASM ctr a -> ASM ctr a
repfor init branch code = mdo
    init
    start <- here
    res <- code
    branch start
    return res

skip :: Show ctr => (ctr -> ASM ctr ()) -> ASM ctr a -> ASM ctr a
skip branch code = mdo
    branch end
    res <- code
    end <- here
    return res

infixl 1 >>. 
cmp >>. branch = (cmp >>) . branch


data Res a = Res a a deriving (Show, Eq, Ord)
start (Res x _) = x
size (Res _ x) = x
end (Res start size) = start + size

resources :: Num a => a -> [a] -> [Res a]
resources start [] = []
resources start (size:sizes) = Res start size : resources (start + size) sizes

provide :: (Num ctr, Eq ctr, Show ctr) => Res ctr -> ASM ctr a -> ASM ctr a
provide res code = mdo
    enforce_counter (start res)
    ret <- code
    enforce_counter (end res)
    return ret

provide_at :: (Num ctr, Eq ctr, Show ctr) => ctr -> Res ctr -> ASM ctr a -> ASM ctr a
provide_at off res code = mdo
    enforce_counter (start res + off)
    ret <- code
    enforce_counter (end res + off)
    return ret

merge_res :: (Num a, Eq a) => [Res a] -> Res a
merge_res = foldl1 merge2 where
    merge2 a b = if end a == start b
        then Res (start a) (size a + size b)
        else error$ "Tried to merge resources that didn't match."

instance Num a => Num (Res a) where
    (+) = error$ "Can't (+) Res."
    (*) = error$ "Can't (*) Res."
    (-) = error$ "Can't (-) Res."
    abs = error$ "Can't abs Res."
    signum = error$ "Can't signum Res."
    fromInteger x = Res (fromInteger x) 0

instance Enum a => Enum (Res a) where
    succ (Res st sz) = Res (succ st) sz
    pred (Res st sz) = Res (pred st) sz
    toEnum = error$ "Can't toEnum to get Res."
    fromEnum = error$ "Can't fromEnum Res (you can toInteger it though."

instance Real a => Real (Res a) where
    toRational = toRational . start

instance Integral a => Integral (Res a) where
    quotRem = error$ "Can't quotRem Res.  It's only Integral for its toInteger."
    toInteger (Res s _) = toInteger s

 -- Kinda icky but okay
instance Bounded a => Bounded (Res a) where
    minBound = Res minBound minBound
    maxBound = Res maxBound maxBound

