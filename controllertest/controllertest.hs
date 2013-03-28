
{-# LANGUAGE RecursiveDo #-}

import qualified Data.ByteString as B
import ASM
import ASM6502
import NES
import Data.Word
import Data.Bits ((.|.), shiftL)

main = do
    B.putStr code

(code, _, data_begin) = asm 0 top

top = mdo
    NES.header 0x01 0x01 0x00 0x00
    data_begin <- prgbank
    chrbank "controllertest/sprites.bin"
    chrbank "controllertest/background.bin"
    return data_begin

prgbank = mdo
    set_counter 0xc000
    begin <- here
    (nmi, reset, irq, data_begin) <- pad (0x4000 - 6) 0xff prg_main
    le16 (fromIntegral nmi)
    le16 (fromIntegral reset)
    le16 (fromIntegral irq)
    return data_begin

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
ball_y = 0x11
ball_xscreen = 0x12
ball_yscreen = 0x13
camera_x = 0x14
camera_y = 0x15
camera_xscreen = 0x16
camera_yscreen = 0x17
ball_screen_x = 0x18
ball_screen_y = 0x19

xcoord = 0x00
ycoord = 0x01

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
    let bump GT = inc
        bump LT = dec
        unbump GT = dec
        unbump LT = inc
        branch GT = bcc
        branch LT = bcs
        move bit coord dir thr = mdo
            lda input1
            andi bit
            skip beq $ do
                bump dir (ball_x + coord)
                lda (ball_x + coord)
                sec
                sbc (camera_x + coord)
                sta (ball_screen_x + coord)
                cmpi thr
                skip (branch dir) $ do
                    bump dir (camera_x + coord)
                    unbump dir (ball_screen_x + coord)
    move btn_left  xcoord LT 0x40
    move btn_right xcoord GT 0xc1
    move btn_up    ycoord LT 0x40
    move btn_down  ycoord GT 0xb1


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
        repfor (ldyi (size all_palettes - 1)) (dey >>. bpl) $ mdo
            lday all_palettes
            sta ppu_mem
         -- Draw background
        lda ppu_status
        0x20 ->* ppu_address
        0x00 ->* ppu_address
         -- name table
        repfor (ldxi 0x00) (cpxi (size background) >>. bne) $ mdo
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

    data_begin <- here

    provide sprite_palettes $ hexdata$ ""
        ++ "2212020f"
        ++ "2a1a0a0f"
        ++ "2616060f"
        ++ "3010000f"

    provide background_palettes $ hexdata$ ""
        ++ "2212020f"
        ++ "2a1a0a0f"
        ++ "2616060f"
        ++ "3010000f"

    provide btnspr_x $ hexdata$
        "d0 bf c8 c8 e0 d8 e8 f0"
    provide btnspr_y $ hexdata$
        "d0 d0 d8 c8 d0 d0 d0 d0"
    provide btnspr_tile $ hexdata$
        "01 01 00 00 03 03 02 02"
    provide btnspr_attr $ hexdata$
        "40 00 80 00 00 00 01 01"

    provide tiles_tl $ hexdata$
        "00 01 02 01 04 06 06"
    provide tiles_tr $ hexdata$
        "00 01 01 03 06 05 06"
    provide tiles_bl $ hexdata$
        "00 06 04 06 04 06 06"
    provide tiles_br $ hexdata$
        "00 06 06 05 06 05 06"

    provide background $ hexdata$ ""
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

    return (nmi, reset, 0, data_begin)

[sprite_palettes, background_palettes, btnspr_x, btnspr_y, btnspr_tile, btnspr_attr,
 tiles_tl, tiles_tr, tiles_bl, tiles_br, background] = res6502 data_begin
 [16, 16, 8, 8, 8, 8, 7, 7, 7, 7, 0xf0]

all_palettes :: Res6502
all_palettes = merge_res [sprite_palettes, background_palettes]


