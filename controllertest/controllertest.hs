
{-# LANGUAGE RecursiveDo #-}

import qualified Data.ByteString as B
import ASM
import ASM6502
import qualified NES
import Data.Word
import Data.Bits ((.|.), shiftL)
import Data.Monoid
import Debug.Trace


main = do
    B.putStr $ NES.header 0x01 0x01 0x00 0x00
    B.putStr $ prgbank
    sprites <- B.readFile "controllertest/sprites.bin"
    B.putStr $ sprites
    B.putStr $ B.replicate (0x1000 - B.length sprites) 0xff
    background <- B.readFile "controllertest/background.bin"
    B.putStr $ background
    B.putStr $ B.replicate (0x1000 - B.length background) 0xff

 -- Keeps track of how many sprites have been drawn so far.
 -- Only valid during drawing phase.
sprites_left = 0x0f

 -- ZERO PAGE ALLOCATIONS
 -- Let's have a ball that's moved by the arrow keys.
 -- That's original, right?

ball_x : ball_y :
 camera_x : camera_y :
 _ = [0x10..0xff] :: [Word8]

 -- MAIN MEMORY ALLOCATIONS

save_ppu_ctrl : input1 : input2 : _ = [0x0300..0x0800] :: [Word16]

 -- UTILITY VALUES

btn_right : btn_left : btn_down : btn_up :
 btn_start : btn_select : btn_b : btn_a :
 _ = map (shiftL 1) [0..] :: [Word8]

xcoord = 0x00
ycoord = 0x01

(prgbank, 0, data_begin) = asm 0 $ mdo
    set_counter 0xc000
    reset <- startof reset_section
    nmi <- startof nmi_section
    data_begin <- startof data_section
    fillto 0xfffa 0xff
    provide NES.nmi_vector $ le16 nmi
    provide NES.reset_vector $ le16 reset
    provide NES.irq_vector $ le16 0
    return data_begin

init_ball = mdo
    ldai 0x80
    sta ball_x
    sta ball_y

move_ball = mdo
    let bump GT = inc
        bump LT = dec
        unbump GT = dec
        unbump LT = inc
        branch GT = bcc
        branch LT = bcs
            
        move bit coord dir thr move_camera = mdo
            lda input1
            andi bit
            skip beq $ do
                bump dir (ball_x + coord)
                lda (ball_x + coord)
                sub (camera_x + coord)
                cmpi thr
                skip (branch dir) move_camera

    move btn_left xcoord LT 0x40 $ do
        dec camera_x
        skip (lda camera_x >> cmpi 0xff >>. bne) $ do
            NES.nametable_x_bit -^>* save_ppu_ctrl
    move btn_right xcoord GT 0xc1 $ do
        inc camera_x
        skip bne $ do
            NES.nametable_x_bit -^>* save_ppu_ctrl
    move btn_up ycoord LT 0x40 $ do
        dec camera_y
        skip (lda camera_y >> cmpi 0xff >>. bne) $ do
            0xef ->* camera_y
            NES.nametable_y_bit -^>* save_ppu_ctrl
    move btn_down ycoord GT 0xb1 $ do
        inc camera_y
        skip (lda camera_y >> cmpi 0xf0 >>. bne) $ do
            0x00 ->* camera_y
            NES.nametable_y_bit -^>* save_ppu_ctrl

draw_model :: Res6502 -> ASM6502 ()
draw_model model = do
    fordeyin model $ do
        lday model
        add ball_y
        sub camera_y
        sta NES.spr_mem
        dey
        lday model
        sta NES.spr_mem
        dey
        lday model
        sta NES.spr_mem
        dey
        lday model
        add ball_x
        sub camera_x
        sta NES.spr_mem
        dec sprites_left

 -- Main code stuff

read_controllers = mdo
     -- Freeze controllers for polling
    0x01 ->* NES.controller1
    0x00 ->* NES.controller1
    let read port bits = mdo
            repfor (ldxi 0x07) (dex >>. bpl) $ mdo
                lda port
                lsra
                rol bits
    read NES.controller1 input1
    read NES.controller2 input2

reset_section = mdo
    NES.initialize
     -- Load all the palettes
    NES.set_ppu_address 0x3f00
    fordeyin all_palettes $ mdo
        lday all_palettes
        sta NES.ppu_mem
     -- Draw background
    NES.set_ppu_address 0x2000
     -- name table
    repfor (ldxi 0x00) (cpxi (size background) >>. bne) $ mdo
        let col = 0x00
            tmpx = 0x01
         -- top row
        stx tmpx
        repfor (0x10 ->* col) (dec col >>. bne) $ mdo
            ldyx background
            lday tiles_tl
            sta NES.ppu_mem
            lday tiles_tr
            sta NES.ppu_mem
            inx
        ldx tmpx
         -- bottom row
        repfor (0x10 ->* col) (dec col >>. bne) $ mdo
            ldyx background
            lday tiles_bl
            sta NES.ppu_mem
            lday tiles_br
            sta NES.ppu_mem
            inx
     -- attribute table
    ldai 0xAA
    repfor (ldyi 0x40) (dey >>. bne) $ mdo
        sta NES.ppu_mem
     -- enable rendering
    save_ppu_ctrl *<- NES.enable_nmi_bit .|. NES.background_1000_bit
    sta NES.ppu_ctrl
    NES.ppu_mask *<- NES.dont_clip_background_bit .|. NES.dont_clip_sprites_bit
                 .|. NES.enable_background_bit .|. NES.enable_sprites_bit
    
    init_ball
     -- Done with everything
    idle <- here
    jmp idle

nmi_section = mdo
    read_controllers
     -- Start sprite memory transfer
    0x00 ->* NES.spr_address
    0x40 ->* sprites_left
     -- Draw the buttons
    let input_tmp = 0x00
    input1 *->* input_tmp
    repfor (ldxi 0x07) (dex >>. bpl) $ mdo
        ldax btnspr_y >> sta NES.spr_mem
        ldax btnspr_tile >> sta NES.spr_mem
        ldax btnspr_attr >> mdo
            asl input_tmp
            skip bcs $ mdo
                orai 0x03
            sta NES.spr_mem
        ldax btnspr_x >> sta NES.spr_mem
        dec sprites_left
     -- Draw the ball
    move_ball
    draw_model ball_model
     -- Stow away any unused sprites
    ldai 0xfe
    rep (dec sprites_left >>. bne) $ mdo
        sta NES.spr_mem
        sta NES.spr_mem
        sta NES.spr_mem
        sta NES.spr_mem
     -- Set the bg scroll
    save_ppu_ctrl *->* NES.ppu_ctrl
    lda NES.ppu_status
    camera_x *->* NES.ppu_scroll
    camera_y *->* NES.ppu_scroll
    rti

[sprite_palettes, background_palettes, ball_model, btnspr_x, btnspr_y, btnspr_tile, btnspr_attr,
 tiles_tl, tiles_tr, tiles_bl, tiles_br, background] = res6502 data_begin
 [16, 16, 4 * 4, 8, 8, 8, 8, 7, 7, 7, 7, 0xf0]

all_palettes :: Res6502
all_palettes = sprite_palettes <> background_palettes

data_section = mdo

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

    provide ball_model $ hexdata$ ""
        ++ "08 41 06 08"
        ++ "08 41 05 00"
        ++ "00 01 06 08"
        ++ "00 01 05 00"

    provide btnspr_x $ hexdata "d0 bf c8 c8 e0 d8 e8 f0"
    provide btnspr_y $ hexdata "d0 d0 d8 c8 d0 d0 d0 d0"
    provide btnspr_tile $ hexdata "01 01 00 00 03 03 02 02"
    provide btnspr_attr $ hexdata "40 00 80 00 00 00 01 01"

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


