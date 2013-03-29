
{-# LANGUAGE RecursiveDo #-}

 -- Please import this qualified.
module NES.ASoundEngine where

import Data.Word
import ASM
import ASM6502
import NES

square1 = 0x00
square2 = 0x08

datasize = 0x10

chn_program = (+ 0)
chn_pos = (+ 2)
chn_timer = (+ 4)


validate (Allocation _ s) cont = if s == datasize
    then cont
    else error$ "Sound engine was given an allocation of the wrong size (" ++ show s ++ " /= " ++ show datasize ++ ")"

init engine = validate engine $ mdo
    let init_part nesch ch = do
        0x00 ->* chn_pos ch
        0x04 ->* chn_timer ch
        0x30 ->* channel_env nesch
    init_part NES.pulse1 (start engine + square1)
    init_part NES.pulse2 (start engine + square2)

set_program engine chn prog = validate engine $ do
    low prog ->* start engine + chn
    high prog ->* start engine + chn + 1

run engine note_table = mdo
    let run_part nesch ch = mdo
        let timer = chn_timer ch
            pos = chn_pos ch
            program = chn_program ch
            env = NES.channel_env nesch
            low = NES.channel_low nesch
            high = NES.channel_high nesch
        dec timer
        skip bne $ mdo
            ldy pos
            read_note <- here
            ldayp program
            bmi special
            note <- startof$ mdo
                asla
                tax
                ldax note_table
                sta low
                ldax (start note_table + 1)
                sta high
                iny
                ldayp program
                sta timer
                iny
                jmp done_sound
            special <- startof$ mdo
                iny
                cmpi repeat_code >> beq do_repeat
                cmpi set_env_code >> beq do_set_env
                do_repeat <- startof$ mdo
                    ldyi 0x00
                    jmp read_note
                do_set_env <- startof$ mdo
                    ldayp program
                    sta env
                    iny
                    jmp read_note
                nothing
            done_sound <- here
            sty pos
    run_part NES.pulse1 (start engine + square1)
    run_part NES.pulse2 (start engine + square2)


repeat_code : set_env_code : _ = [0x80..] :: [Word8]

repeat = [repeat_code]
set_env val = [set_env_code, val]
