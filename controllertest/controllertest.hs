
{-# LANGUAGE RecursiveDo #-}

import qualified Data.ByteString as B
import ASM
import ASM6502
import NES

main = do
    B.putStr (assemble_asm top)

top = mdo
    NES.header 0x01 0x01 0x00 0x00
    prgbank
    chrbank "controllertest/sprites.bin"
    chrbank "controllertest/background.bin"

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

chrbank :: String -> ASM ()
chrbank file = mdo
    size <- sizeof$ binfile file
    fill (0x1000 - size) 0xff

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
    repfor (ldxi 0x00) bne dex $ mdo
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
    (nmi, reset, irq) <- prg_main
    end <- here
    fill (0x4000 - (end - begin) - 6) 0xff
    le16 (fromIntegral nmi)
    le16 (fromIntegral reset)
    le16 (fromIntegral irq)

prg_main = mdo
    
    idle <- startof$ jmp idle

    reset <- startof$ mdo
        initialize
         -- Load all the palettes
        lda ppu_status
        0x3f ->* ppu_address
        0x00 ->* ppu_address
        repfor (ldyi 0x1f) bpl dey $ mdo
            lday sprite_palettes
            sta ppu_mem
         -- Draw background
        lda ppu_status
        0x20 ->* ppu_address
        0x00 ->* ppu_address
         -- name table
        repfor (ldxi 0x00) bne (cpxi 0xf0) $ mdo
            let col = 0x00
                tmpx = 0x01
             -- top row
            stx tmpx
            repfor (0x10 ->* col) bne (dec col) $ mdo
                ldyxm background
                lday tiles_tl
                sta ppu_mem
                lday tiles_tr
                sta ppu_mem
                inx
            ldx tmpx
             -- bottom row
            repfor (0x10 ->* col) bne (dec col) $ mdo
                ldyxm background
                lday tiles_bl
                sta ppu_mem
                lday tiles_br
                sta ppu_mem
                inx
         -- attribute table
        lda 0xAA
        repfor (ldyi 0x40) bne dey $ mdo
            sta ppu_mem
         -- enable rendering
        0x90 ->* ppu_ctrl  -- 10010000 enable nmi, bg at ppu0x1000
        0x1e ->* ppu_mask  -- 00011110
        jmp idle

    nmi <- startof$ mdo
        read_controllers
         -- Start sprite memory transfer
        let sprite_count = 0x01
        0x00 ->* spr_address
        ldai 0x40
        sta sprite_count
         -- Draw the buttons
        let input_tmp = 0x00
        input1 *->* input_tmp
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
         -- Set the bg scroll
        lda ppu_status
        ldai 0x00
        sta ppu_scroll
        sta ppu_scroll
        rti

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

    tiles_tl <- startof$ hexdata$
        "00 01 02 01 04 06 06"
    tiles_tr <- startof$ hexdata$
        "00 01 01 03 06 05 06"
    tiles_bl <- startof$ hexdata$
        "00 06 04 06 04 06 06"
    tiles_br <- startof$ hexdata$
        "00 06 06 05 06 05 06"

    background <- startof$ hexdata$ ""
        ++ "00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00"
        ++ "00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00"
        ++ "00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00"
        ++ "00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00"
        ++ "00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00"
        ++ "00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00"
        ++ "00 00 00 00 00 00 00 00 00 00 02 01 01 03 00 00"
        ++ "00 00 00 00 00 00 00 00 00 00 04 06 06 05 00 00"
        ++ "00 00 00 00 00 00 00 00 00 00 04 06 06 05 00 00"
        ++ "00 00 00 00 00 00 00 00 00 00 04 06 06 05 00 00"
        ++ "00 00 02 01 01 01 03 00 00 00 04 06 06 05 00 00"
        ++ "00 00 04 06 06 06 05 00 00 00 04 06 06 05 00 00"
        ++ "01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01"
        ++ "06 06 06 06 06 06 06 06 06 06 06 06 06 06 06 06"
        ++ "06 06 06 06 06 06 06 06 06 06 06 06 06 06 06 06"

    return (nmi, reset, 0)

