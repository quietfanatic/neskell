
{-# LANGUAGE RecursiveDo #-}

import qualified Data.ByteString as B
import ASM
import ASM6502
import NES

main = B.putStr (assemble_asm top)

top = mdo
    NES.header 0x01 0x01 0x00
    prgbank
    chrbank

button_a = 0x80
button_b = 0x40
button_select = 0x20
button_start = 0x10
button_up = 0x08
button_down = 0x04
button_left = 0x02
button_right = 0x01

input1 = 0x0100
input2 = 0x0101

chrbank = bytes (B.readFile "chrbank.bin")

initialize = mdo
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
    rep bpl $ mdo
        bit ppu_status
     -- clear memory
    ldxi 0x00
    rep bne $ mdo
        ldai 0x00
        stax 0x00
        stax 0x0100
        stax 0x0300
        stax 0x0400
        stax 0x0500
        stax 0x0600
        stax 0x0700
        ldai 0xff
        stax 0x0200
        dex
     -- wait for second vblank
    rep bpl $ mdo
        bit ppu_status

read_controllers = mdo
     -- Freeze controllers for polling
    0x01 ->* controller1
    0x00 ->* controller1
    let read port bits =
        repfor (ldxi 0x07) bpl dex $ mdo
            lda port
            lsra
            rol bits
    read controller1 input1
    read controller2 input2

prgbank = mdo
    set_counter 0xc000
    begin <- here
    (reset, nmi, irq) <- prg_main
    end <- here
    fill (0x4000 - (end - begin) - vector_size) 0xff
    vector_size <- sizeof$ mdo
        le16 (fromIntegral reset)
        le16 (fromIntegral nmi)
        le16 (fromIntegral irq)

prg_main = mdo

    reset <- startof$ mdo
        initialize
         -- Load all the palettes
        lda ppu_status
        0x3f ->* ppu_address
        0x00 ->* ppu_address
        repfor (ldyi 0x1f) bpl dey $ mdo
            lday sprite_palettes
            sta ppu_mem
             -- enable rendering
            0x90 ->* ppu_ctrl  -- 10010000 enable nmi, bg at ppu0x1000
            0x1e ->* ppu_mask  -- 00011110

    nmi <- startof$ mdo
        read_controllers

    sprite_palettes <- startof$ hexdata$ ""
        ++ "2212020f"
        ++ "2a1a0a0f"
        ++ "2616060f"
        ++ "3010000f"

    background_palettes <- startof$ hexdata$ ""
        ++ "2212020f"
        ++ "2a1a0a0f"
        ++ "2616060f"
        ++ "3010000f"

    return (reset, nmi, 0)

