
{-# LANGUAGE RecursiveDo #-}

import qualified Data.ByteString as B
import ASM
import ASM6502
import NES

main = do
    str <- B.readFile "controllertest/chrbank.bin"
    B.putStr (assemble_asm (top str))

top :: B.ByteString -> ASM ()
top str = mdo
    NES.header 0x01 0x01 0x00
    prgbank
    chrbank str

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

chrbank :: B.ByteString -> ASM ()
chrbank str = mdo
    size <- sizeof$ bytestring str
    fill (0x2000 - size) 0xff

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
    let read port bits = mdo
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
    fill (0x4000 - (end - begin) - 6) 0xff
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
         -- Start sprite memory transfer
        let sprite_count = 0x01
        0x00 ->* spr_address
        sta sprite_count  -- Counts down from 0x100 (really 0x00)
         -- Draw the buttons
        lda input1
        let input_tmp = 0x00
        sta input_tmp
        repfor (ldxi 0x07) bpl dex $ mdo
            ldaxm btnspr_y
            sta spr_mem
            ldaxm btnspr_tile
            sta spr_mem
            ldaxm btnspr_attr
            asl input_tmp
            skip bcs $ mdo
                orai 0x03
            sta spr_mem
            ldaxm btnspr_x
            sta spr_mem
            dec sprite_count
         -- Stow away any unused sprites
        ldai 0xfe
        rep bne $ mdo
            sta spr_mem
            sta spr_mem
            sta spr_mem
            sta spr_mem
            dec sprite_count

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

    btnspr_x <- startof$ hexdata$
        "d0 bf c8 c8 e0 d8 e8 f0"
    btnspr_y <- startof$ hexdata$
        "d0 d0 d8 c8 d0 d0 d0 d0"
    btnspr_tile <- startof$ hexdata$
        "01 01 00 00 03 03 02 02"
    btnspr_attr <- startof$ hexdata$
        "40 00 80 00 00 00 01 01"

    return (reset, nmi, 0)

