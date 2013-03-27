module NES where

import Data.Word
import ASM
import ASM6502

 -- Provides an ines header.
 --       mapper   prgs     chrs
header :: Word8 -> Word8 -> Word8 -> ASM ()
header mapper prgs chrs = do
    ascii "NES"
    byte 0x1a
    byte mapper
    byte prgs
    byte chrs
    fill 9 0

 -- The names of various memory-mapped ports

ppu_ctrl = 0x2000
ppu_mask = 0x2001
ppu_status = 0x2002
spr_address = 0x2003
spr_mem = 0x2004
ppu_scroll = 0x2005
ppu_address = 0x2006
ppu_mem = 0x2007

channel_env = (+ 0)
channel_low = (+ 2)
channel_high = (+ 3)

square1 = 0x4000
square2 = 0x4004

square1_env = 0x4000
square1_low = 0x4002
square1_high = 0x4003
square2_env = 0x4004
square2_low = 0x4006
square2_high = 0x4007

sprite_dma = 0x4014
apu_flags = 0x4015
controller1 = 0x4016
controller2 = 0x4017
apu_ctrl = 0x4017


