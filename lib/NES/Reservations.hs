
{-# LANGUAGE DeriveDataTypeable #-}

module NES.Reservations (res, resz) where

import Assembler
import ASM
import ASM6502
import Data.Typeable
import Data.Word
import Text.Printf

newtype ResCounter a = ResCounter a deriving (Typeable)

res_generic :: (Typeable a, Integral a, Integral b) => String -> a -> a -> b -> ASM6502 (ASMSection a ())
res_generic space_name space_start space_end size = do
    rescounter <- get_annotation
    errmess <- generate_fail_message (printf "Not enough %s space left to reserve 0x%x bytes" space_name (toInteger size))
    let alloc_size = fromIntegral size
        alloc_start = case rescounter of
            Nothing -> space_end - alloc_size
            Just (ResCounter alloc_end) -> if alloc_end - alloc_size >= space_start
                then alloc_end - alloc_size
                else error errmess
        ret = allocate1 (fromIntegral alloc_start) alloc_size
    set_annotation (Just (ResCounter alloc_start))
    return ret

res :: Integral a => a -> ASM6502 (ASMSection Word16 ())
res = res_generic "main memory" 0x300 0x800
resz :: Integral a => a -> ASM6502 (ASMSection Word8 ())
resz = res_generic "zero page" 0x10 0x00


