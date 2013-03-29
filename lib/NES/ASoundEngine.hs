
{-# LANGUAGE RecursiveDo #-}

 -- Please import this qualified.
module NES.ASoundEngine where

import Data.Word
import ASM
import ASM6502
import NES

square1 = 0x00
square2 = 0x04
triangle = 0x08
noise = 0x0c

datasize = 0x20

 -- Data is laid out in fours to match the NES channels
 -- 11112222ttttnnnn11--22--tt--nn--
program (Allocation x _) = x + 0x00
position (Allocation x _) = x + 0x02
timer (Allocation x _) = x + 0x10

validate (Allocation _ s) cont = if s == datasize
    then cont
    else error$ "Sound engine was given an allocation of the wrong size (" ++ show s ++ " /= " ++ show datasize ++ ")"

init engine = validate engine $ mdo
    let init_part chn = do
        0x00 ->* program engine + chn
        0x01 ->* timer engine + chn
        0x00 ->* (NES.channel_env (NES.channels + chn))
    init_part square1
    init_part square2

set_program engine chn prog = validate engine $ do
    ldai (low prog)
    sta (program engine + chn)
    sta (position engine + chn)
    ldai (high prog)
    sta (program engine + chn + 1)
    sta (position engine + chn + 1)

run engine note_table = mdo
     -- X is always the channel offset (0, 4, 8, or c)
     -- Y is either the low end of pos or the note index.
    let eprogram = program engine
        eposition = position engine
        etimer = timer engine
        pos = 0x00  -- 0x00 stays 0 (the "real" pointer is y:0x01)
        tmpy = 0x02
        next = do
            iny
            skip bne (inc (pos + 1))
    ldxi 0x00
    run_one <- startof$ mdo
        decx etimer
        skip bne $ mdo
            ldyx eposition
            ldax (eposition + 1) >> sta (pos + 1)
            read_note <- here
            ldayp pos
            bmi special
            note <- startof$ mdo
                sty tmpy
                asla
                tay
                lday note_table >> stax (NES.channel_low NES.channels)
                lday (start note_table + 1) >> stax (NES.channel_high NES.channels)
                ldy tmpy
                next
                ldayp pos >> stax etimer
                next
                jmp done_sound
            special <- startof$ mdo
                next
                cmpi repeat_code >> beq do_repeat
                cmpi set_env_code >> beq do_set_env
                do_repeat <- startof$ mdo
                    ldax eprogram >> sta pos
                    ldax (eprogram + 1) >> sta (pos + 1)
                    jmp read_note
                do_set_env <- startof$ mdo
                    ldayp pos >> stax (NES.channel_env NES.channels)
                    next
                    jmp read_note
                nothing
            done_sound <- here
            styx eposition
            lda (pos + 1) >> stax (eposition + 1)
    inx >> inx >> inx >> inx
    skip (cpxi 0x08 >>. beq) (jmp run_one)

repeat_code : set_env_code : _ = [0x80..] :: [Word8]

repeat = [repeat_code]
set_env val = [set_env_code, val]
