
{-# LANGUAGE RecursiveDo #-}

import Data.Word
import Data.Bits
import qualified Data.ByteString as B
import ASM
import ASM6502
import NES
import Debug.Trace

main = do
    B.putStr $ NES.header 0x01 0x00 0x00 0x00
    B.putStr $ prgbank

channel_program = (+ 0)
channel_pos = (+ 2)
channel_timer = (+ 4)

square1_state = 0x90
square2_state = 0x98

(prgbank, 0, data_begin) = asm 0 $ mdo
    set_counter 0xc000
    reset <- startof reset_section
    nmi <- startof nmi_section
    data_begin <- startof data_section
    fillto 65530 0xff
    provide nmi_vector $ le16 nmi
    provide reset_vector $ le16 reset
    provide irq_vector $ le16 0
    return data_begin

init_sound_engine ch program = mdo
    low program ->* (channel_program ch)
    high program ->* (channel_program ch) + 1
    0x00 ->* channel_pos ch
    0x04 ->* channel_timer ch

sound_engine nesch ch note_table default_env = mdo
    let timer = channel_timer ch
        pos = channel_pos ch
        program = channel_program ch
        env = NES.channel_env nesch
        low = NES.channel_low nesch
        high = NES.channel_high nesch
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
            ldax note_table
            sta low
            ldax (start note_table + 1)
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

reset_section = mdo
    initialize

     -- Set background color
    lda ppu_status
    0x3f ->* ppu_address
    0x00 ->* ppu_address
    0x0f ->* ppu_mem

     -- nmi for the sound
    ppu_ctrl *<- ppu_enable_nmi
    ppu_mask *<- 0

     -- enable sound
    0x30 ->* square1_env  -- 00110000
    0x30 ->* square2_env  -- 00110000
    0x03 ->* apu_flags  -- 00000011

    init_sound_engine square1_state square1_program
    init_sound_engine square2_state square2_program

    idle <- here
    jmp idle

nmi_section = mdo

    sound_engine NES.square1 square1_state note_table 0x33
    sound_engine NES.square2 square2_state note_table 0x32

    rti

[note_table, square1_program, square2_program]
    = res6502 data_begin [2 * 0x5e, length square1_program', length square2_program']

 -- this was initially copypasted from http://www.nintendoage.com/forum/messageview.cfm?catid=22&threadid=22776
 -- but a couple tweaks may have been made to sharpen notes up a little
note_table' = [                                                             0x07f1, 0x0780, 0x0713, -- a1-b1 (0x00-0x02)
    0x06ad, 0x064d, 0x05f3, 0x059d, 0x054d, 0x0500, 0x04b8, 0x0475, 0x0435, 0x03f8, 0x03bf, 0x0389, -- c2-b2 (0x03-0x0e)
    0x0356, 0x0326, 0x02f9, 0x02ce, 0x02a6, 0x027f, 0x025c, 0x023a, 0x021a, 0x01fb, 0x01df, 0x01c4, -- c3-b3 (0x0f-0x1a)
    0x01ab, 0x0193, 0x017c, 0x0167, 0x0151, 0x013f, 0x012d, 0x011c, 0x010c, 0x00fd, 0x00ef, 0x00e1, -- c4-b4 (0x1b-0x26)
    0x00d2, 0x00c9, 0x00bd, 0x00b3, 0x00a9, 0x009f, 0x0096, 0x008e, 0x0086, 0x007e, 0x0077, 0x0070, -- c5-b5 (0x27-0x32)
    0x006a, 0x0064, 0x005e, 0x0059, 0x0054, 0x004f, 0x004b, 0x0046, 0x0042, 0x003f, 0x003b, 0x0038, -- c6-b6 (0x33-0x3e)
    0x0034, 0x0031, 0x002f, 0x002c, 0x0029, 0x0027, 0x0025, 0x0023, 0x0021, 0x001f, 0x001d, 0x001b, -- c7-b7 (0x3f-0x4a)
    0x001a, 0x0018, 0x0017, 0x0015, 0x0014, 0x0013, 0x0012, 0x0011, 0x0010, 0x000f, 0x000e, 0x000d, -- c8-b8 (0x4b-0x56)
    0x000c, 0x000c, 0x000b, 0x000a, 0x000a, 0x0009, 0x0008] :: [Word16]                             -- c9-f#9 (0x57-0x5d)

square1_program' = hex $ ""
    ++ "401f 4021 4022 4026 4024 2022 2021 1c1f 0400 101f 101d 341a 0c00"
    ++ "401f 4022 4021 401d 541f 0c00 2026 6424 1c00"
    ++ "0000"

square2_program' = hex $ ""
    ++ "8013 8011 400f 4011 8013"
    ++ "800f 8011 8013 8011"
    ++ "0000"

data_section = mdo
    provide note_table $ sequence $ map le16 $ note_table'
    provide square1_program $ bytes square1_program'
    provide square2_program $ bytes square2_program'
        

