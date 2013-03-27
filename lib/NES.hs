module NES where

import Data.Word
import Data.Bits
import ASM

 -- Provides an ines header.
 --       prgs     chrs     mapper   flags
header :: Word8 -> Word8 -> Word8 -> Word8 -> ASM ()
header prgs chrs mapper flags = do
    ascii "NES"
    byte 0x1a
    byte prgs
    byte chrs
    byte (shiftL (mapper .&. 0xf) 4 .|. (flags .&. 0xf))
    byte ((mapper .&. 0xf0) .|. shiftR (flags .&. 0xf0) 4)
    fill 8 0

 -- The names of various memory-mapped ports

ppu_ctrl = 0x2000 :: Word16
ppu_inc_32 = bit 2 :: Word8
ppu_sprites_1000 = bit 3 :: Word8
ppu_background_1000 = bit 4 :: Word8
ppu_sprites_8x16 = bit 5 :: Word8
ppu_enable_nmi = bit 7 :: Word8

ppu_mask = 0x2001 :: Word16
ppu_grayscale = bit 0 :: Word8
ppu_dont_clip_background = bit 1 :: Word8
ppu_dont_clip_sprites = bit 2 :: Word8
ppu_enable_background = bit 3 :: Word8
ppu_enable_sprites = bit 4 :: Word8
ppu_intensify_red = bit 5 :: Word8
ppu_intensify_green = bit 6 :: Word8
ppu_intensify_blue = bit 7 :: Word8

ppu_status = 0x2002 :: Word16
spr_address = 0x2003 :: Word16
spr_mem = 0x2004 :: Word16
ppu_scroll = 0x2005 :: Word16
ppu_address = 0x2006 :: Word16
ppu_mem = 0x2007 :: Word16

channel_env = (+ 0) :: Word16 -> Word16
channel_low = (+ 2) :: Word16 -> Word16
channel_high = (+ 3) :: Word16 -> Word16

square1 = 0x4000 :: Word16
square2 = 0x4004 :: Word16

square1_env = 0x4000 :: Word16
square1_low = 0x4002 :: Word16
square1_high = 0x4003 :: Word16
square2_env = 0x4004 :: Word16
square2_low = 0x4006 :: Word16
square2_high = 0x4007 :: Word16

sprite_dma = 0x4014 :: Word16
apu_flags = 0x4015 :: Word16
controller1 = 0x4016 :: Word16
controller2 = 0x4017 :: Word16
apu_ctrl = 0x4017 :: Word16


