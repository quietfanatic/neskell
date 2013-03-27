
{-# LANGUAGE RecursiveDo #-}

import ASM
import Data.Char
import qualified Data.Bits as DB
import ASM6502
import qualified Data.ByteString as B


main = B.putStr (assemble_asm top)

top = mdo
    ascii "NES"
    bytes [0x1a, 0x01, 0x01, 0x01, 0x00]
    fill 8 0x00
    prgbank0
    prgbank1
    chrbank

chrbank = fill 0x2000 0xff

prgbank0 = fill 0x2000 0xff

ppuctrl = 0x2000
ppumask = 0x2001
ppustatus = 0x2002
spraddress = 0x2003
sprmem = 0x2004
ppuscroll = 0x2005
ppuaddress = 0x2006
ppumem = 0x2007

type ChannelPorts = Int
chn_env = (+ 0)
chn_low = (+ 2)
chn_high = (+ 3)

chn_square1 = 0x4000
chn_square2 = 0x4004

sq1_env = 0x4000
sq1_low = 0x4002
sq1_high = 0x4003
sq2_env = 0x4004
sq2_low = 0x4006
sq2_high = 0x4007

sprdma = 0x4014
apuflags = 0x4015
controller1 = 0x4016
controller2 = 0x4017
apuctrl = 0x4017

 -- bits representing the buttons
button_a = 0x80
button_b = 0x40
button_select = 0x20
button_start = 0x10
button_up = 0x08
button_down = 0x04
button_left = 0x02
button_right = 0x01

frame256 = 0x80

type Channel_State = Int

channel_program = (+ 0)
channel_pos = (+ 2)
channel_timer = (+ 4)

square1 = 0x90
square2 = 0x98

sqr1_program = 0x90
sqr1_pos = 0x92
sqr1_timer = 0x94
sqr2_program = 0x98
sqr2_pos = 0x9a
sqr2_timer = 0x9c

 -- where the controller bitfields are stored
input1 = 0x0100
input2 = 0x0101

init_sound_engine chn ch program = mdo
    low program ->* (channel_program ch)
    high program ->* (channel_program ch) + 1
    0x00 ->* channel_pos ch
    0x04 ->* channel_timer ch

sound_engine chn ch note_table default_env = mdo
    let timer = channel_timer ch
        pos = channel_pos ch
        program = channel_program ch
        env = chn_env chn
        low = chn_low chn
        high = chn_high chn
    dec timer
    skip bne $ mdo
        read_note <- here
        ldy pos
        ldayp program
        beq special
        sta timer
        iny
        ldayp program
        beq rest
        tone <- startof$ mdo
            asla
            tax
            ldaxm note_table
            sta low
            ldaxm (note_table+1)
            sta high
            default_env ->* env  -- 00110011
            iny
            jmp done_sound
        rest <- startof$ mdo
            0x30 ->* env  -- 00110000
            iny
            jmp done_sound
        special <- startof$ mdo
            iny
            ldayp program
            beq repeat
            unknown <- startof$ mdo
                iny
                jmp done_special
            repeat <- startof$ mdo
                iny
                sta pos
                jmp done_special
            done_special <- startof$ mdo
                jmp read_note
            nothing
        done_sound <- here
        sty pos


initialize = mdo
    sei
    cld
    0x40 ->* 0x4017  -- disable apu frame irq
    ldxi 0xff
    txs        -- make stack
    inx
    stx ppuctrl  -- disable nmi
    stx ppumask  -- disable rendering
    stx 0x4010  -- disable dmc irqs

     -- wait for first vblank
    rep bpl $ mdo
        bit ppustatus
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
        bit ppustatus


prgbank1 = mdo
    set_counter 0xe000
    bank_begin <- here
    reset <- here

    initialize

     -- Load a palette set
    lda ppustatus
    0x3f ->* ppuaddress
    0x00 ->* ppuaddress
    repfor (ldyi 0x1f) bpl dey $ mdo
        lday sprite_palettes
        sta ppumem

    mdo  -- initialize background (doesn't seem to be working)
        lda ppustatus
        0x20 ->* ppuaddress
        ldxi 0x00
        stx ppuaddress

        ldai 0x00
        repfor (ldyi 0x40) bne dey $ mdo
            repfor (ldxi 0xf0) bne dex $ mdo
                sta ppumem

        ldai 0xaa
        repfor (ldxi 0x40) bne dex $ mdo
            sta ppumem

     -- enable rendering
    0x90 ->* ppuctrl  -- 10010000 enable nmi, background at ppu0x1000
    0x1e ->* ppumask  -- 00011110

     -- enable sound
    0x30 ->* sq1_env  -- 00110000
    0x30 ->* sq2_env  -- 00110000
    0x03 ->* apuflags  -- 00000011

    init_sound_engine chn_square1 square1 square1_program
    init_sound_engine chn_square2 square2 square2_program

    idle <- here
    jmp idle

    nmi <- here
    inc frame256

     -- read controllers
    0x01 ->* controller1
    0x00 ->* controller1  -- controller poll sequence
    repfor (ldxi 0x07) bpl dex $ mdo
        lda controller1
        lsra
        rol input1
    repfor (ldxi 0x07) bpl dex $ mdo
        lda controller2
        lsra
        rol input2

    sound_engine chn_square1 square1 note_table 0x33
    sound_engine chn_square2 square2 note_table 0x32

    done <- here
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

     -- this was initially copypasted from http://www.nintendoage.com/forum/messageview.cfm?catid=22&threadid=22776
     -- but a couple tweaks may have been made to sharpen notes up a little
    note_table <- startof$ sequence$ map le16 [                                 0x07f1, 0x0780, 0x0713, -- a1-b1 (0x00-0x02)
        0x06ad, 0x064d, 0x05f3, 0x059d, 0x054d, 0x0500, 0x04b8, 0x0475, 0x0435, 0x03f8, 0x03bf, 0x0389, -- c2-b2 (0x03-0x0e)
        0x0356, 0x0326, 0x02f9, 0x02ce, 0x02a6, 0x027f, 0x025c, 0x023a, 0x021a, 0x01fb, 0x01df, 0x01c4, -- c3-b3 (0x0f-0x1a)
        0x01ab, 0x0193, 0x017c, 0x0167, 0x0151, 0x013f, 0x012d, 0x011c, 0x010c, 0x00fd, 0x00ef, 0x00e1, -- c4-b4 (0x1b-0x26)
        0x00d2, 0x00c9, 0x00bd, 0x00b3, 0x00a9, 0x009f, 0x0096, 0x008e, 0x0086, 0x007e, 0x0077, 0x0070, -- c5-b5 (0x27-0x32)
        0x006a, 0x0064, 0x005e, 0x0059, 0x0054, 0x004f, 0x004b, 0x0046, 0x0042, 0x003f, 0x003b, 0x0038, -- c6-b6 (0x33-0x3e)
        0x0034, 0x0031, 0x002f, 0x002c, 0x0029, 0x0027, 0x0025, 0x0023, 0x0021, 0x001f, 0x001d, 0x001b, -- c7-b7 (0x3f-0x4a)
        0x001a, 0x0018, 0x0017, 0x0015, 0x0014, 0x0013, 0x0012, 0x0011, 0x0010, 0x000f, 0x000e, 0x000d, -- c8-b8 (0x4b-0x56)
        0x000c, 0x000c, 0x000b, 0x000a, 0x000a, 0x0009, 0x0008]                                          -- c9-f#9 (0x57-0x5d)


    square1_program <- startof$ hexdata$ ""
        ++ "401f 4021 4022 4026 4024 2022 2021 1c1f 0400 101f 101d 341a 0c00"
        ++ "401f 4022 4021 401d 541f 0c00 2026 6424 1c00"
        ++ "0000"
        
    square2_program <- startof$ hexdata$ ""
        ++ "8013 8011 400f 4011 8013"
        ++ "800f 8011 8013 8011"
        ++ "0000"

    let irq = 0
    bank_end <- here
    fill (0x2000 - 6 - (bank_end - bank_begin)) 0xff

     -- the interrupt vector must be at the end of the bank.
    sequence$ map (le16 . fromIntegral) [nmi, reset, irq]



startof x = do
    start <- here
    x
    return start

endof x = x >> here

sizeof x = do
    start <- here
    x
    end <- here
    return (end - start)


rep :: (Int -> ASM ()) -> ASM () -> ASM ()
rep branch code = mdo
    start <- here
    code
    branch start

repfor :: ASM () -> (Int -> ASM ()) -> ASM () -> ASM () -> ASM ()
repfor init branch inc code = mdo
    init
    start <- here
    code
    inc
    branch start

skip :: (Int -> ASM ()) -> ASM () -> ASM ()
skip branch code = mdo
    branch end
    code
    end <- here
    nothing

infix 2 ->*
infix 2 *->*
val ->* mem = ldai val >> sta mem
a *->* b = lda a >> sta b

