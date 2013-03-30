
{-# LANGUAGE RecursiveDo #-}

import Data.Word
import Data.Bits
import qualified Data.ByteString as B
import ASM
import ASM6502
import qualified NES
import Debug.Trace
import qualified NES.ASoundEngine as S

main = do
    B.putStr $ NES.header 0x01 0x00 0x00 0x00
    B.putStr $ prgbank


[sound] = allocate 0x300 [S.datasize]

(prgbank, 0, data_begin) = asm 0 $ mdo
    set_counter 0xc000
    reset <- startof reset_section
    nmi <- startof nmi_section
    data_begin <- startof data_section
    fillto 65530 0xff
    provide NES.nmi $ le16 nmi
    provide NES.reset $ le16 reset
    provide NES.irq $ le16 0
    return data_begin

reset_section = mdo
    NES.initialize

     -- Set background color
    NES.set_ppuaddr 0x3f00
    0x0f ->* NES.ppudata

     -- nmi for the sound
    NES.ppuctrl *<- NES.enable_nmi_bit
    NES.ppumask *<- 0

     -- enable sound
    NES.apuctrl *<- NES.enable_pulse1 .|. NES.enable_pulse2 .|. NES.enable_triangle
    NES.apumode *<- NES.sequencer_mode_bit .|. NES.disable_frame_irq_bit

    S.init sound
    S.set_program sound NES.pulse1 pulse1_program
    S.set_program sound NES.pulse2 pulse2_program
    S.set_program sound NES.triangle triangle_program

    idle <- here
    jmp idle

nmi_section = mdo

    S.run sound note_table

    rti

[note_table, pulse1_program, pulse2_program, triangle_program]
    = allocate16 data_begin [2 * 0x5f, length pulse1_program2, length pulse2_program2, length triangle_program2]

 -- this was initially copypasted from http://www.nintendoage.com/forum/messageview.cfm?catid=22&threadid=22776
 -- but a couple tweaks may have been made to sharpen notes up a little
note_table' = [                                                     0x0000, 0x07f1, 0x0780, 0x0713, -- a1-b1 (0x01-0x03)
    0x06ad, 0x064d, 0x05f3, 0x059d, 0x054d, 0x0500, 0x04b8, 0x0475, 0x0435, 0x03f8, 0x03bf, 0x0389, -- c2-b2 (0x04-0x0f)
    0x0356, 0x0326, 0x02f9, 0x02ce, 0x02a6, 0x027f, 0x025c, 0x023a, 0x021a, 0x01fb, 0x01df, 0x01c4, -- c3-b3 (0x10-0x1b)
    0x01ab, 0x0193, 0x017c, 0x0167, 0x0151, 0x013f, 0x012d, 0x011c, 0x010c, 0x00fd, 0x00ef, 0x00e1, -- c4-b4 (0x1c-0x27)
    0x00d2, 0x00c9, 0x00bd, 0x00b3, 0x00a9, 0x009f, 0x0096, 0x008e, 0x0086, 0x007e, 0x0077, 0x0070, -- c5-b5 (0x28-0x33)
    0x006a, 0x0064, 0x005e, 0x0059, 0x0054, 0x004f, 0x004b, 0x0046, 0x0042, 0x003f, 0x003b, 0x0038, -- c6-b6 (0x34-0x3f)
    0x0034, 0x0031, 0x002f, 0x002c, 0x0029, 0x0027, 0x0025, 0x0023, 0x0021, 0x001f, 0x001d, 0x001b, -- c7-b7 (0x40-0x4b)
    0x001a, 0x0018, 0x0017, 0x0015, 0x0014, 0x0013, 0x0012, 0x0011, 0x0010, 0x000f, 0x000e, 0x000d, -- c8-b8 (0x4c-0x57)
    0x000c, 0x000c, 0x000b, 0x000a, 0x000a, 0x0009, 0x0008] :: [Word16]                             -- c9-f#9 (0x58-0x5e)

pulse1_program' = S.set_env (NES.duty_quarter .|. NES.disable_length_counter .|. NES.constant_volume .|. 0x8)
    ++ hex "2040 2240 2340 2740 2540 2320 2220 201c 0004 2010 1e10 1b34 000c"
    ++ hex "2040 2340 2240 1e40 2054 000c 2720 2564 001c"
    ++ S.repeat

pulse2_program' = S.set_env (NES.duty_quarter .|. NES.disable_length_counter .|. NES.constant_volume .|. 0x6)
    ++ hex "1480 1280 1040 1240 1480"
    ++ hex "1080 1280 1480 1280"
    ++ S.repeat

pulse1_program2 = S.set_env (NES.duty_half .|. NES.disable_length_counter .|. 0x3)
    ++ hex "3814 0004 3414 0004 3814 0004 3414 0004 3814 0004 3414 0004 3714 0004 3414 0004"
    ++ S.loop 2 35
    ++ hex "3814 0004 3414 0004 3814 0004 3414 0004 3714 0004 3414 0004 3714 0004 3414 0004"
    ++ S.loop 4 35
    ++ S.repeat
pulse2_program2 = S.set_env (NES.duty_half .|. NES.disable_length_counter .|. 0x3)
    ++ hex "3114 0004 2c14 0004 3114 0004 2c14 0004 3114 0004 2c14 0004 3014 0004 2b14 0004"
    ++ S.loop 2 35
    ++ hex "3114 0004 2c14 0004 3114 0004 2c14 0004 3014 0004 2b14 0004 3014 0004 2b14 0004"
    ++ S.loop 4 35
    ++ S.repeat
triangle_program2 = S.set_env 0x81
    ++ hex "00c0 00c0"
    ++ hex "0030 1412 1912 1c06 0006 1c24 1330 000c"
    ++ hex "0030 1412 1912 1c06 0006 1c24 1e04 1f04 1e04 1c24 000c"
    ++ hex "0030 1412 1912 1c06 0006 1c24 1324 1506 0006 1430 009c"
    ++ S.repeat


data_section = mdo
    provide note_table $ sequence $ map le16 $ note_table'
    provide pulse1_program $ bytes pulse1_program2
    provide pulse2_program $ bytes pulse2_program2
    provide triangle_program $ bytes triangle_program2

