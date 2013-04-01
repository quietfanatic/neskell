
{-# LANGUAGE RecursiveDo #-}

import Data.Word
import Data.Bits
import qualified Data.ByteString as B
import Assembler
import ASM
import ASM6502
import qualified NES
import Debug.Trace
import qualified NES.ASoundEngine as S

main = do
    B.putStr $ NES.header 0x01 0x00 0x00 0x00
    B.putStr $ asm_result prgbank


(_, prgbank) = asm 0xc000 $ mdo
    let [bg_color] = allocate8 0x10 [1]

    sound <- S.a_sound_engine' note_table

    reset <- sect "reset" $ mdo
        NES.initialize'

         -- Set background color
        NES.set_ppuaddr 0x3f00
        0x0f ->* NES.ppudata
        sta bg_color

         -- nmi for the sound
        NES.ppuctrl *<- NES.enable_nmi_bit
        NES.ppumask *<- 0

         -- enable sound
        NES.apuctrl *<- NES.enable_pulse1 .|. NES.enable_pulse2 .|. NES.enable_triangle
        NES.apumode *<- NES.sequencer_mode_bit .|. NES.disable_frame_irq_bit


        S.initialize' sound
        S.set_stream' sound NES.pulse1 pulse1_stream2
        S.set_stream' sound NES.pulse2 pulse2_stream2
        S.set_stream' sound NES.triangle triangle_stream2

        idle <- here
        jmp idle

    nmi <- sect "nmi" $ mdo
        NES.set_ppuaddr 0x3f00
        start bg_color *->* NES.ppudata

        S.run' sound

        rti

    set_bg_blue <- sect "set_bg_blue" $ mdo
        0x02 ->* start bg_color
        rts
    set_bg_orange <- sect "set_bg_orange" $ mdo
        0x08 ->* start bg_color
        rts

     -- this was initially copypasted from http://www.nintendoage.com/forum/messageview.cfm?catid=22&threadid=22776
     -- but a couple tweaks may have been made to sharpen notes up a little
    note_table <- sect "note_table" $ mapM_ le16 ([                     0x0000, 0x07f1, 0x0780, 0x0713, -- a1-b1 (0x01-0x03)
        0x06ad, 0x064d, 0x05f3, 0x059d, 0x054d, 0x0500, 0x04b8, 0x0475, 0x0435, 0x03f8, 0x03bf, 0x0389, -- c2-b2 (0x04-0x0f)
        0x0356, 0x0326, 0x02f9, 0x02ce, 0x02a6, 0x027f, 0x025c, 0x023a, 0x021a, 0x01fb, 0x01df, 0x01c4, -- c3-b3 (0x10-0x1b)
        0x01ab, 0x0193, 0x017c, 0x0167, 0x0151, 0x013f, 0x012d, 0x011c, 0x010c, 0x00fd, 0x00ef, 0x00e1, -- c4-b4 (0x1c-0x27)
        0x00d2, 0x00c9, 0x00bd, 0x00b3, 0x00a9, 0x009f, 0x0096, 0x008e, 0x0086, 0x007e, 0x0077, 0x0070, -- c5-b5 (0x28-0x33)
        0x006a, 0x0064, 0x005e, 0x0059, 0x0054, 0x004f, 0x004b, 0x0046, 0x0042, 0x003f, 0x003b, 0x0038, -- c6-b6 (0x34-0x3f)
        0x0034, 0x0031, 0x002f, 0x002c, 0x0029, 0x0027, 0x0025, 0x0023, 0x0021, 0x001f, 0x001d, 0x001b, -- c7-b7 (0x40-0x4b)
        0x001a, 0x0018, 0x0017, 0x0015, 0x0014, 0x0013, 0x0012, 0x0011, 0x0010, 0x000f, 0x000e, 0x000d, -- c8-b8 (0x4c-0x57)
        0x000c, 0x000c, 0x000b, 0x000a, 0x000a, 0x0009, 0x0008] :: [Word16])                            -- c9-f#9 (0x58-0x5e)

    pulse1_stream1 <- sect "pulse1_stream1" $ do
        S.set_env (NES.duty_quarter .|. NES.disable_length_counter .|. NES.constant_volume .|. 0x8)
        S.call set_bg_blue
        S.repeat $ do
            hexdata "2040 2240 2340 2740 2540 2320 2220 201c 0004 2010 1e10 1b34 000c"
            hexdata "2040 2340 2240 1e40 2054 000c 2720 2564 001c"

    pulse2_stream1 <- sect "pulse2_stream1" $ do
        S.set_env (NES.duty_quarter .|. NES.disable_length_counter .|. NES.constant_volume .|. 0x6)
        S.repeat $ do
            hexdata "1480 1280 1040 1240 1480"
            hexdata "1080 1280 1480 1280"

    pulse1_stream2 <- sect "pulse1_stream2" $ do
        S.set_env (NES.duty_half .|. NES.disable_length_counter .|. 0x3)
        S.call set_bg_orange
        S.loop 2 $ do
            S.loop 3 $ hexdata "3814 0004 3414 0004"
            hexdata "3714 0004 3414 0004"
        S.repeat $ do
            S.loop 4 $ do
                hexdata "3814 0004 3414 0004 3814 0004 3414 0004 3714 0004 3414 0004 3714 0004 3414 0004"
            S.loop 4 $ do
                hexdata "380c 2f0c 340c 2f0c 380c 2f0c 340c 2f0c 370c 300c 340c 300c 370c 300c 340c 300c"
    pulse2_stream2 <- sect "pulse2_stream2" $ do
        S.set_env (NES.duty_half .|. NES.disable_length_counter .|. 0x3)
        S.loop 2 $ do
            hexdata "3114 0004 2c14 0004 3114 0004 2c14 0004 3114 0004 2c14 0004 3014 0004 2b14 0004"
        S.repeat $ do
            S.loop 4 $ do
                hexdata "3114 0004 2c14 0004 3114 0004 2c14 0004 3014 0004 2b14 0004 3014 0004 2b14 0004"
            let volume x = S.set_env (NES.duty_half .|. NES.disable_length_counter .|. NES.constant_volume .|. x :: Word8)
            volume 3  -- The order of the volume and the loop is on purpose.
            S.loop 3 $ do
                S.note 0x2c 0x18 >> volume 3 >> S.delay 0x18 >> volume 4 >> S.delay 0x18 >> volume 5 >> S.delay 0x18
                volume 4 >> S.note 0x2b 0x24 >> volume 3 >> S.note 0x2a 0x24 >> volume 2 >> S.note 0x28 0x18
            S.delay (0x30 * 4)
            S.set_env (NES.duty_half .|. NES.disable_length_counter .|. 0x3)
    triangle_stream2 <- sect "triangle_stream2" $ do
        S.set_env 0x81
        hexdata "00c0 00c0"
        S.repeat $ do
            hexdata "0030 1412 1912 1c06 0006 1c24 1330 000c"
            hexdata "0030 1412 1912 1c06 0006 1c24 1e04 1f04 1e04 1c24 000c"
            hexdata "0030 1412 1912 1c06 0006 1c24 1324 1506 0006 1430 009c"
            S.loop 2 $ do
                hexdata "1c30 2330 2430 2b1a 0016"
            hexdata "1c30 2330 2430 2b30 2a30 2330 2130 2830"

    fillto 0xfffa 0xff
    provide NES.nmi $ le16 nmi
    provide NES.reset $ le16 reset
    provide NES.irq $ le16 0

