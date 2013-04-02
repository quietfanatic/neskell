
{-# LANGUAGE RecursiveDo #-}

import Assembler
import ASM
import ASM6502
import NES
import NES.ImageLoader
import qualified Data.ByteString as B
import qualified Actors as A
import Data.Bits

main = do
    B.putStr $ NES.header 0x01 0x01 0x00 0x00
    B.putStr $ asm_result prgbank
    sprites <- file_to_chr greyscale_palette "agame/sprites.png"
    B.putStr $ sprites
    B.putStr $ B.replicate (0x1000 - B.length sprites) 0xff
    background <- file_to_chr greyscale_palette "agame/background.png"
    B.putStr $ background
    B.putStr $ B.replicate (0x1000 - B.length background) 0xff


(_, prgbank) = asm 0xc000 $ mdo
    actors <- A.actors' 1 actor_models
    reset <- sect "reset" $ do
        NES.initialize'
        NES.set_ppuaddr NES.vram_palettes
        forinyin palettes $ do
            lday palettes
            sta NES.ppudata
        lda (NES.enable_background_bit .|. NES.enable_sprites_bit)
        sta NES.ppumask
        lda (NES.background_1000_bit .|. NES.enable_nmi_bit)
        sta NES.ppuctrl
    nmi <- sect "nmi" $ do
        A.draw_actors' actors

    palettes <- section $ do
        hexdata "0f 07 0a 1a"
        hexdata "0f 07 0a 1a"
        hexdata "0f 07 0a 1a"
        hexdata "0f 07 0a 1a"
        hexdata "0f 2d 00 10"
        hexdata "0f 2d 00 10"
        hexdata "0f 2d 00 10"
        hexdata "0f 2d 00 10"
    robot_model <- section $ do
        byte (size robot_model)
        hexdata "00 00 00 00"
        hexdata "08 01 00 00"
        hexdata "10 02 00 00"
        hexdata "00 08 00 08"
        hexdata "08 09 00 08"
        hexdata "10 0a 00 08"
        hexdata "18 0b 00 08"
    actor_models <- section $
        le16 robot_model
    fillto 0xfffa 0xff
    provide NES.nmi $ le16 nmi
    provide NES.reset $ le16 reset
    provide NES.irq $ le16 0

