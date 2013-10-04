
{-# LANGUAGE RecursiveDo #-}

import Assembler
import ASM
import ASM6502
import NES
import NES.ImageLoader
import NES.Reservations
import NES.Header001
import qualified Data.ByteString as B
import qualified Actors as A
import Data.Bits

main = do
     -- 1 PRG, 1 CHR, mapper 1, has sram
    B.putStr $ NES.header 0x01 0x01 0x01 0x02
    B.putStr $ asm_result prgbank
    sprites <- file_to_chr greyscale_palette "agame/sprites.png"
    B.putStr $ sprites
    B.putStr $ B.replicate (0x1000 - B.length sprites) 0xff
    background <- file_to_chr greyscale_palette "agame/background.png"
    B.putStr $ background
    B.putStr $ B.replicate (0x1000 - B.length background) 0xff


(_, prgbank) = asm 0xc000 $ mdo
    actors <- A.actors' 1 actor_models
    framecounter <- resz 1
    reset <- sect "reset" $ do
        NES.initialize'
        NES.Header001.reset'
        NES.Header001.write' 0x8000 0x03  -- Horizontal mirroring
        NES.Header001.writeA0' 0xe000  -- Enable WRAM
         -- Load palettes
        NES.set_ppuaddr NES.vram_palettes
        forinyin palettes $ do
            lday palettes
            sta NES.ppudata
         -- For testing purposes, let's try writing to sram
        ldyi 0x00
        ldxi 0x00
        rep (iny >>. bne) $ do
            txa
            stay 0x6000
            inx
         -- Show some tiles
        NES.set_ppuaddr (NES.vram_nametable_0 + 36)
        ldyi 0x00 >> sty NES.ppudata
        iny >> sty NES.ppudata
        iny >> sty NES.ppudata
        iny >> sty NES.ppudata
        NES.set_ppuaddr (NES.vram_nametable_0 + 68)
        ldyi 0x00 >> sty NES.ppudata
        iny >> sty NES.ppudata
        iny >> sty NES.ppudata
        iny >> sty NES.ppudata
         -- Set all attributes in nametable 0
        NES.set_ppuaddr NES.vram_attribute_table_0
        ldai 0x50
        sta NES.ppudata
        sta NES.ppudata
        sta NES.ppudata
        sta NES.ppudata
        sta NES.ppudata
        sta NES.ppudata
        sta NES.ppudata
        sta NES.ppudata
         -- Init inital actor
        ldai 0x80
        sta (A.xs actors)
        sta (A.ys actors)
        ldai (NES.background_1000_bit .|. NES.enable_nmi_bit)
        sta NES.ppuctrl
        ldai (NES.enable_background_bit .|. NES.enable_sprites_bit)
        sta NES.ppumask
    idle <- sect "idle" (jmp idle)
    nmi <- sect "nmi" $ do
        A.start_draw' actors
        A.draw_actors' actors
        A.finish_draw' actors
        lda NES.ppustatus
        ldai 0
        sta NES.ppuscroll
        sta NES.ppuscroll
        inc framecounter
        rti

    palettes <- section $ do
        hexdata "0f 1d 2d 3d"
        hexdata "0f 10 20 30"
        hexdata "0f 07 0a 1a"
        hexdata "0f 07 0a 1a"
        hexdata "0f 1d 2d 3d"
        hexdata "0f 1d 2d 3d"
        hexdata "0f 1d 2d 3d"
        hexdata "0f 1d 2d 3d"
    robot_model <- section $ do
        byte (size robot_model)
        hexdata "00 00 00 00"
        hexdata "08 01 00 00"
        hexdata "10 02 00 00"
        hexdata "00 08 00 08"
        hexdata "08 09 00 08"
        hexdata "10 0a 00 08"
    actor_models <- section $
        le16 robot_model
    fillto 0xfffa 0xff
    provide NES.nmi $ le16 nmi
    provide NES.reset $ le16 reset
    provide NES.irq $ le16 0

