
{-# LANGUAGE DeriveDataTypeable #-}

module NES.Reservations (res, resz) where

import Assembler
import ASM
import ASM6502
import Data.Typeable
import Data.Word
import Text.Printf

data ResCounter a = ResCounter a Bool deriving (Typeable)

res_generic :: (Typeable a, Integral a, Integral b) => String -> a -> a -> b -> ASM6502 (Section a ())
res_generic space_name space_start space_end size = do
    ResCounter alloc_end started <- get_annotation_default (ResCounter space_end False)
    errmess <- generate_fail_message (printf "Not enough %s space left to reserve 0x%x bytes" space_name (toInteger size))
    let alloc_size = fromIntegral size
        alloc_start = if (not started) || space_start + alloc_size <= alloc_end
            then alloc_end - alloc_size
            else error errmess
        ret = allocate1 alloc_start alloc_size
    set_annotation (ResCounter alloc_start True)
    return ret

res :: Integral a => a -> ASM6502 (Section Word16 ())
res = res_generic "main memory" 0x300 0x800
resz :: Integral a => a -> ASM6502 (Section Word8 ())
resz = res_generic "zero page" 0x10 0x00


