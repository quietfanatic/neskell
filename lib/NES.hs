module NES where

import Data.Word
import ASM
import ASM6502 hiding (bit)
import Data.Bits
import Data.Char
import qualified Data.ByteString as B

 -- Provides an ines header.
 --       prgs     chrs     mapper   flags
header :: Word8 -> Word8 -> Word8 -> Word8 -> B.ByteString
header prgs chrs mapper flags = let asc = fromIntegral . ord in B.pack [
    asc 'N', asc 'E', asc 'S', 0x1a,
    prgs, chrs,
    shiftL (mapper .&. 0xf) 4 .|. (flags .&. 0xf),
    (mapper .&. 0xf0) .|. shiftR (flags .&. 0xf0) 4,
    0, 0, 0, 0, 0, 0, 0, 0
        ]

 -- The names of various memory-mapped ports

ppu_ctrl = 0x2000 :: Word16
nametable_x_bit = bit 0 :: Word8
nametable_y_bit = bit 1 :: Word8
inc_32_bit = bit 2 :: Word8
sprites_1000_bit = bit 3 :: Word8
background_1000_bit = bit 4 :: Word8
sprites_8x16_bit = bit 5 :: Word8
enable_nmi_bit = bit 7 :: Word8

ppu_mask = 0x2001 :: Word16
grayscale_bit = bit 0 :: Word8
dont_clip_background_bit = bit 1 :: Word8
dont_clip_sprites_bit = bit 2 :: Word8
enable_background_bit = bit 3 :: Word8
enable_sprites_bit = bit 4 :: Word8
intensify_red_bit = bit 5 :: Word8
intensify_green_bit = bit 6 :: Word8
intensify_blue_bit = bit 7 :: Word8

ppu_status = 0x2002 :: Word16

spr_addr = 0x2003 :: Word16
spr_mem = 0x2004 :: Word16

ppu_scroll = 0x2005 :: Word16

ppu_addr = 0x2006 :: Word16
set_ppu_addr w16 = do
    lda ppu_status
    high w16 ->* ppu_addr
    low w16 ->* ppu_addr
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

initialize_begin = do
    sei
    cld
    0x40 ->* 0x4017  -- disable apu frame irq
    ldxi 0xff
    txs        -- make stack
    inx
    stx ppu_ctrl  -- disable nmi
    stx ppu_mask  -- disable rendering
    stx 0x4010  -- disable dmc irqs

     -- wait for first vblank
    rep bpl (bitm ppu_status)

clear_memory = do
    repfor (ldxi 0x00) (dex >>. bne) $ do
        ldai 0x00
        stax 0x00
        stax 0x0100
        stax 0x0200
        stax 0x0300
        stax 0x0400
        stax 0x0500
        stax 0x0600
        stax 0x0700

initialize_end = do
     -- wait for second vblank
    rep bpl (bitm ppu_status)

initialize_custom_clear clear = initialize_begin >> clear >> initialize_end
initialize = initialize_custom_clear clear_memory

[nmi_vector, reset_vector, irq_vector] = res6502 0xfffa [2, 2, 2]

