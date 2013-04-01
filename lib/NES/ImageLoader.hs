module NES.ImageLoader (file_to_chr, bytestring_to_chr, image_to_chr, greyscale_palette) where

import Data.Bits
import Data.Word
import qualified Data.ByteString as B
import qualified Codec.Picture as P
import qualified Codec.Picture.Types as PT

greyscale_palette :: (P.PixelRGBA8 -> Int)
greyscale_palette (P.PixelRGBA8 r g b a) = let
    total = r + g + b
    in    if total <= (255 * 3 `div` 4) then 0
     else if total <= (255 * 6 `div` 4) then 1
     else if total <= (255 * 9 `div` 4) then 2
     else                                    3

bits_to_bytes :: [Bool] -> [Word8]
bits_to_bytes [] = []
bits_to_bytes (b0:b1:b2:b3:b4:b5:b6:b7:rest) =
    foldl (\acc b -> shiftL acc 1 .|. if b then 1 else 0) 0 [b0,b1,b2,b3,b4,b5,b6,b7] : bits_to_bytes rest
bits_to_bytes weird = error $ "Got a weird number of bits: " ++ show (length weird)

image_to_chr :: Bits output => (P.PixelRGBA8 -> output) -> P.DynamicImage -> B.ByteString
image_to_chr pal dynimg = let
    img :: P.Image P.PixelRGBA8
    img = case dynimg of
        P.ImageY8 i -> PT.promoteImage i
        P.ImageYA8 i -> PT.promoteImage i
        P.ImageRGB8 i -> PT.promoteImage i
        P.ImageRGBA8 i -> PT.promoteImage i
        P.ImageYCbCr8 i -> error$ "Sorry, NES.ImageLoader cannot use image in YCbCr8 format."
    width = P.imageWidth img
    height = P.imageHeight img
    cols = width `div` 8
    rows = height `div` 8
    blocks = [(x, y) | x <- [0..cols-1], y <- [0..rows-1]]
    pxblocks = [[P.pixelAt img (bx * 8 + px) (by * 8 + py) | py <- [0..7], px <- [0..7]] | (bx, by) <- blocks]
    indexblocks = [[pal p | p <- ps] | ps <- pxblocks]
    bitfields b = [[testBit i b | i <- is] | is <- indexblocks]
    bits = map (uncurry (++)) $ zip (bitfields 0) (bitfields 1)
    bytes = concatMap bits_to_bytes bits
    in B.pack bytes

e_image_to_chr pal (Left mess) = error mess
e_image_to_chr pal (Right img) = image_to_chr pal img

bytestring_to_chr :: Bits output => (P.PixelRGBA8 -> output) -> B.ByteString -> B.ByteString
bytestring_to_chr pal = e_image_to_chr pal . P.decodeImage

file_to_chr :: Bits output => (P.PixelRGBA8 -> output) -> FilePath -> IO B.ByteString
file_to_chr pal = fmap (e_image_to_chr pal) . P.readImage

