
{-# LANGUAGE RecursiveDo #-}

import qualified Data.ByteString as B
import Assembly
import ASM
import ASM6502
import qualified NES
import Data.Word
import Data.Bits ((.|.), shiftL)
import Data.Monoid
import Debug.Trace


main = do
    B.putStr $ NES.header 0x01 0x01 0x00 0x00
    B.putStr $ asm_result prgbank
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

save_ppuctrl : input1 : input2 : _ = [0x0300..0x0800] :: [Word16]

 -- UTILITY VALUES

btn_right : btn_left : btn_down : btn_up :
 btn_start : btn_select : btn_b : btn_a :
 _ = map (shiftL 1) [0..] :: [Word8]

xcoord = 0x00
ycoord = 0x01

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
            NES.nametable_x_bit -^>* save_ppuctrl
    move btn_right xcoord GT 0xc1 $ do
        inc camera_x
        skip bne $ do
            NES.nametable_x_bit -^>* save_ppuctrl
    move btn_up ycoord LT 0x40 $ do
        dec camera_y
        skip (lda camera_y >> cmpi 0xff >>. bne) $ do
            0xef ->* camera_y
            NES.nametable_y_bit -^>* save_ppuctrl
    move btn_down ycoord GT 0xb1 $ do
        inc camera_y
        skip (lda camera_y >> cmpi 0xf0 >>. bne) $ do
            0x00 ->* camera_y
            NES.nametable_y_bit -^>* save_ppuctrl

 -- draw_model_sub: Y = model size in bytes, 00:01 = pointer to model, 02 = xcoord, 03 = ycoord
draw_model_sub = do
    let modelp = 0x00
        xcoord = 0x02
        ycoord = 0x03
    dey
    rep bpl $ do
        ldayp modelp
        add ycoord
        sub camera_y
        sta NES.oamdata
        dey
        ldayp modelp
        sta NES.oamdata
        dey
        ldayp modelp
        sta NES.oamdata
        dey
        ldayp modelp
        add xcoord
        sub camera_x
        sta NES.oamdata
        dec sprites_left
        dey
    rts


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

 -- Main code stuff
prgbank_start = asm 0xc000 nothing

reset = asm prgbank_start $ mdo
    NES.initialize
     -- Load all the palettes
    NES.set_ppuaddr 0x3f00
    fordeyin all_palettes $ mdo
        lday all_palettes
        sta NES.ppudata
     -- Draw background
    NES.set_ppuaddr 0x2000
     -- name table
    repfor (ldxi 0x00) (cpxi (size background) >>. bne) $ mdo
        let col = 0x00
            tmpx = 0x01
         -- top row
        stx tmpx
        repfor (0x10 ->* col) (dec col >>. bne) $ mdo
            ldyx background
            lday tiles_tl
            sta NES.ppudata
            lday tiles_tr
            sta NES.ppudata
            inx
        ldx tmpx
         -- bottom row
        repfor (0x10 ->* col) (dec col >>. bne) $ mdo
            ldyx background
            lday tiles_bl
            sta NES.ppudata
            lday tiles_br
            sta NES.ppudata
            inx
     -- attribute table
    ldai 0xAA
    repfor (ldyi 0x40) (dey >>. bne) $ mdo
        sta NES.ppudata
     -- enable rendering
    save_ppuctrl *<- NES.enable_nmi_bit .|. NES.background_1000_bit
    sta NES.ppuctrl
    NES.ppumask *<- NES.dont_clip_background_bit .|. NES.dont_clip_sprites_bit
                .|. NES.enable_background_bit .|. NES.enable_sprites_bit

    init_ball
     -- Done with everything
    idle <- here
    jmp idle

nmi = asm reset $ mdo
    read_controllers
     -- Start sprite memory transfer
    0x00 ->* NES.oamaddr
    0x40 ->* sprites_left
     -- Draw the buttons
    let input_tmp = 0x00
    input1 *->* input_tmp
    repfor (ldxi 0x07) (dex >>. bpl) $ mdo
        ldax btnspr_y >> sta NES.oamdata
        ldax btnspr_tile >> sta NES.oamdata
        ldax btnspr_attr >> mdo
            asl input_tmp
            skip bcs $ mdo
                orai 0x03
            sta NES.oamdata
        ldax btnspr_x >> sta NES.oamdata
        dec sprites_left
     -- Draw the ball
    move_ball
    low ball_model ->* 0x00
    high ball_model ->* 0x01
    ball_x *->* 0x02
    ball_y *->* 0x03
    ldyi (size ball_model)
    jsr draw_model
     -- Stow away any unused sprites
    ldai 0xfe
    rep (dec sprites_left >>. bne) $ mdo
        sta NES.oamdata
        sta NES.oamdata
        sta NES.oamdata
        sta NES.oamdata
     -- Set the bg scroll
    save_ppuctrl *->* NES.ppuctrl
    lda NES.ppustatus
    camera_x *->* NES.ppuscroll
    camera_y *->* NES.ppuscroll
    rti

    draw_model <- startof draw_model_sub
    nothing

data_begin = asm nmi nothing

[sprite_palettes, background_palettes, ball_model, btnspr_x, btnspr_y, btnspr_tile, btnspr_attr,
 tiles_tl, tiles_tr, tiles_bl, tiles_br, background] = allocate16 data_begin
 [16, 16, 4 * 4, 8, 8, 8, 8, 7, 7, 7, 7, 0xf0]

all_palettes = section_merge sprite_palettes background_palettes

data_section = asm data_begin $ mdo

    provide sprite_palettes $ hexdata$ ""
        ++ "22 12 02 0f"
        ++ "2a 1a 0a 0f"
        ++ "26 16 06 0f"
        ++ "30 10 00 0f"

    provide background_palettes $ hexdata$ ""
        ++ "22 12 02 0f"
        ++ "2a 1a 0a 0f"
        ++ "26 16 06 0f"
        ++ "30 10 00 0f"

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

data_end = asm data_section nothing

prgbank_end = asm data_end $ mdo
    fillto 0xfffa 0xff
    provide NES.nmi $ le16 nmi
    provide NES.reset $ le16 reset
    provide NES.irq $ le16 0

prgbank = asm prgbank_end nothing
