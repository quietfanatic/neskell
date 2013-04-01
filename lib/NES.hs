module NES where

import Data.Word
import Assembler
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

 -- PPU PORTS

ppuctrl = 0x2000 :: Word16
nametable_x_bit = bit 0 :: Word8
nametable_y_bit = bit 1 :: Word8
inc_32_bit = bit 2 :: Word8
sprites_1000_bit = bit 3 :: Word8
background_1000_bit = bit 4 :: Word8
sprites_8x16_bit = bit 5 :: Word8
enable_nmi_bit = bit 7 :: Word8

ppumask = 0x2001 :: Word16
grayscale_bit = bit 0 :: Word8
dont_clip_background_bit = bit 1 :: Word8
dont_clip_sprites_bit = bit 2 :: Word8
enable_background_bit = bit 3 :: Word8
enable_sprites_bit = bit 4 :: Word8
intensify_red_bit = bit 5 :: Word8
intensify_green_bit = bit 6 :: Word8
intensify_blue_bit = bit 7 :: Word8

ppustatus = 0x2002 :: Word16
sprite_overflow_bit = bit 5 :: Word8
sprite_0_hit_but = bit 6 :: Word8
vblank_bit = bit 7 :: Word8

oamaddr = 0x2003 :: Word16
oamdata = 0x2004 :: Word16

ppuscroll = 0x2005 :: Word16

ppuaddr = 0x2006 :: Word16
set_ppuaddr w16 = do
    lda ppustatus
    high w16 ->* ppuaddr
    low w16 ->* ppuaddr
ppudata = 0x2007 :: Word16

sprite_dma = 0x4014 :: Word16

 -- APU STUFF

chn_env = 0x4000 :: Word16
volume_bits = 0x3f :: Word8
constant_volume = 0x10 :: Word8
disable_length_counter = 0x20 :: Word8
duty_bits = 0xc0 :: Word8
duty_eighth = 0x00 :: Word8
duty_quarter = 0x40 :: Word8
duty_half = 0x80 :: Word8
duty_inverted_quarter = 0xc0 :: Word8

chn_sweep = 0x4001 :: Word16

chn_low = 0x4002 :: Word16

chn_high = 0x4003 :: Word16
period_high_bits = 0x07 :: Word8
length_counter_bits = 0xf8 :: Word8

apuports = 0x4000 :: Word16
pulse1 = 0x0 :: Word16  -- For some reason it's prematurely defaulting to Integer
pulse2 = 0x4 :: Word16
triangle = 0x8 :: Word16
noise = 0xc :: Word16

pulse1_env = 0x4000 :: Word16
pulse1_sweep = 0x4001 :: Word16
pulse1_low = 0x4002 :: Word16
pulse1_high = 0x4003 :: Word16
pulse2_env = 0x4004 :: Word16
pulse2_sweep = 0x4005 :: Word16
pulse2_low = 0x4006 :: Word16
pulse2_high = 0x4007 :: Word16
triangle1_env = 0x4008 :: Word16
triangle1_low = 0x400a :: Word16
triangle1_high = 0x400b :: Word16

dmc_flags = 0x4010 :: Word16
loop_sample_bit = bit 6 :: Word8
enable_dmc_irq_bit = bit 7 :: Word8

apuctrl = 0x4015 :: Word16
enable_pulse1 = bit 0 :: Word8
enable_pulse2 = bit 1 :: Word8
enable_triangle = bit 2 :: Word8
enable_noise = bit 3 :: Word8
enable_dmc = bit 4 :: Word8

controller1 = 0x4016 :: Word16
controller2 = 0x4017 :: Word16
apumode = 0x4017 :: Word16
disable_frame_irq_bit = bit 6 :: Word8
sequencer_mode_bit = bit 7 :: Word8

initialize_begin' = do
    sei
    cld
    disable_frame_irq_bit ->* apumode
    ldxi 0xff
    txs        -- make stack
    inx
    stx ppuctrl  -- disable nmi
    stx ppumask  -- disable rendering
    stx dmc_flags  -- disable dmc irqs

     -- wait for first vblank
    rep bpl (bitm ppustatus)

clear_memory' = do
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

initialize_end' = do
     -- wait for second vblank
    rep bpl (bitm ppustatus)

initialize_custom_clear' clear = sect "NES.initialize" (initialize_begin' >> clear >> initialize_end')
initialize' = initialize_custom_clear' clear_memory'

read_input_to' :: (Integral a, Bounded a) => a -> ASM6502 (Section6502 ())
read_input_to' spot = sect "NES.read_input_to" $ do
     -- Freeze controllers for polling
    0x01 ->* NES.controller1
    0x00 ->* NES.controller1
     -- Roll bits in one at a time
    repfor (ldxi 0x01) (dex >>. bpl) $ do
        repfor (ldyi 0x07) (dey >>. bpl) $ do
            ldax NES.controller1
            lsra
            rolx spot

[btn_right, btn_left, btn_down, btn_up, btn_start, btn_select, btn_b, btn_a] =
    map (shiftL 1) [0..7] :: [Word8]

[nmi, reset, irq] = allocate16 0xfffa [2, 2, 2]


