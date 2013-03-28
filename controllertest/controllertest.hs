
{-# LANGUAGE RecursiveDo #-}

import qualified Data.ByteString as B
import ASM
import ASM6502
import NES
import Data.Word
import Data.Bits ((.|.), shiftL)

main = do
    B.putStr (assemble_asm top)

top = mdo
    NES.header 0x01 0x01 0x00 0x00
    prgbank
    chrbank "controllertest/sprites.bin"
    chrbank "controllertest/background.bin"

prgbank = mdo
    set_counter 0xc000
    begin <- here
    (nmi, reset, irq) <- pad (0x4000 - 6) 0xff prg_main
    le16 (fromIntegral nmi)
    le16 (fromIntegral reset)
    le16 (fromIntegral irq)

chrbank :: String -> ASM6502 ()
chrbank = pad 0x1000 0xff . binfile

 -- Some utility pseudoops
addi x = clc >> adci x
addz x = clc >> adcz x
addm x = clc >> adcm x
add x = clc >> adc x
subi x = sec >> sbci x
subz x = sec >> sbcz x
subm x = sec >> sbcm x
sub x = sec >> sbc x

bitlist :: [Word8]
bitlist = map (shiftL 1) [0..]
[btn_right, btn_left, btn_down, btn_up, btn_start, btn_select, btn_b, btn_a] = take 8 bitlist

input1 = 0x0100
input2 = 0x0101

 -- Keeps track of how many sprites have been drawn so far.
 -- Only valid during drawing phase.
sprites_left = 0x0f

 -- Let's have a ball that's moved by the arrow keys.
 -- That's original, right?

ball_x = 0x10
ball_xscreen = 0x11
ball_y = 0x12
camera_x = 0x13
camera_xscreen = 0x14
camera_y = 0x15
ball_screen_x = 0x16
ball_screen_y = 0x17

init_ball = mdo
    ldai 0x80
    sta ball_x
    sta ball_y
    sta ball_screen_x
    sta ball_screen_y
--    ldai 0x00
--    sta bg_x
--    sta bg_y

move_ball = mdo
    let move bit bump ball thr branch camera unbump screen = mdo
            lda input1
            andi bit
            skip beq $ do
                bump ball
                lda ball
                sec
                sbc camera
                sta screen
                cmpi thr
                skip branch $ do
                    bump camera
                    unbump screen
    move btn_left  dec ball_x 0x40 bcs camera_x inc ball_screen_x
    move btn_right inc ball_x 0xc1 bcc camera_x dec ball_screen_x
    move btn_up    dec ball_y 0x40 bcs camera_y inc ball_screen_y
    move btn_down  inc ball_y 0xb1 bcc camera_y dec ball_screen_y


draw_ball = do
    let part yexpr tile attr xexpr = mdo
            lda ball_screen_y
            yexpr
            sta spr_mem
            tile ->* spr_mem
            attr ->* spr_mem
            lda ball_screen_x
            xexpr
            sta spr_mem
            dec sprites_left
        add8 = clc >> adci 0x08
    part nothing 0x05 0x01 nothing
    part add8 0x06 0x01 nothing
    part nothing 0x05 0x41 add8
    part add8 0x06 0x41 add8

 -- Main code stuff

read_controllers = mdo
     -- Freeze controllers for polling
    0x01 ->* controller1
    0x00 ->* controller1
    let read port bits = mdo
            repfor (ldxi 0x07) (dex >>. bpl) $ mdo
                lda port
                lsra
                rol bits
    read controller1 input1
    read controller2 input2

prg_main = mdo
    
    idle <- startof$ jmp idle

    reset <- startof$ mdo
        initialize
         -- Load all the palettes
        lda ppu_status
        0x3f ->* ppu_address
        0x00 ->* ppu_address
        repfor (ldyi 0x1f) (dey >>. bpl) $ mdo
            lday sprite_palettes
            sta ppu_mem
         -- Draw background
        lda ppu_status
        0x20 ->* ppu_address
        0x00 ->* ppu_address
         -- name table
        repfor (ldxi 0x00) (cpxi 0xf0 >>. bne) $ mdo
            let col = 0x00
                tmpx = 0x01
             -- top row
            stx tmpx
            repfor (0x10 ->* col) (dec col >>. bne) $ mdo
                ldyx background
                lday tiles_tl
                sta ppu_mem
                lday tiles_tr
                sta ppu_mem
                inx
            ldx tmpx
             -- bottom row
            repfor (0x10 ->* col) (dec col >>. bne) $ mdo
                ldyx background
                lday tiles_bl
                sta ppu_mem
                lday tiles_br
                sta ppu_mem
                inx
         -- attribute table
        ldai 0xAA
        repfor (ldyi 0x40) (dey >>. bne) $ mdo
            sta ppu_mem
         -- enable rendering
        ppu_ctrl *<- ppu_enable_nmi .|. ppu_background_1000
        ppu_mask *<- ppu_dont_clip_background .|. ppu_dont_clip_sprites
                 .|. ppu_enable_background .|. ppu_enable_sprites
        
        init_ball
        jmp idle

    nmi <- startof$ mdo
        read_controllers
         -- Start sprite memory transfer
        0x00 ->* spr_address
        0x40 ->* sprites_left
         -- Draw the buttons
        let input_tmp = 0x00
        input1 *->* input_tmp
        repfor (ldxi 0x07) (dex >>. bpl) $ mdo
            ldax btnspr_y
            sta spr_mem
            ldax btnspr_tile
            sta spr_mem
            ldax btnspr_attr
            asl input_tmp
            skip bcs $ mdo
                orai 0x03
            sta spr_mem
            ldax btnspr_x
            sta spr_mem
            dec sprites_left
         -- Draw the ball
        move_ball
        draw_ball
         -- Stow away any unused sprites
        ldai 0xfe
        rep (dec sprites_left >>. bne) $ mdo
            sta spr_mem
            sta spr_mem
            sta spr_mem
            sta spr_mem
         -- Set the bg scroll
        lda ppu_status
        camera_x *->* ppu_scroll
        camera_y *->* ppu_scroll
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

