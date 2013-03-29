
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


[sound] = allocate 0x90 [S.datasize]

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
    0x03 ->* NES.apuctrl  -- 00000011

    S.init sound
    S.set_program sound S.square1 square1_program
    S.set_program sound S.square2 square2_program

    idle <- here
    jmp idle

nmi_section = mdo

    S.run sound note_table

    rti

[note_table, square1_program, square2_program]
    = allocate16 data_begin [2 * 0x5f, length square1_program', length square2_program']

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

square1_program' = []
    ++ hex "4020 4022 4023 4027 4025 2023 2022 1c20 0400 1020 101e 341b 0c00"
    ++ hex "4020 4023 4022 401e 5420 0c00 2027 6425 1c00"
    ++ S.repeat

square2_program' = []
    ++ hex "8014 8012 4010 4012 8014"
    ++ hex "8010 8012 8014 8012"
    ++ S.repeat

data_section = mdo
    provide note_table $ sequence $ map le16 $ note_table'
    provide square1_program $ bytes square1_program'
    provide square2_program $ bytes square2_program'
        

