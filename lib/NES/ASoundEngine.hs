
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
        0x01 ->* chn_timer ch
        0x20 ->* channel_env nesch
    init_part NES.pulse1 (start engine + square1)
    init_part NES.pulse2 (start engine + square2)

set_program engine chn prog = validate engine $ do
    ldai (low prog)
    sta (chn_program (start engine + chn))
    sta (chn_pos (start engine + chn))
    ldai (high prog)
    sta (chn_program (start engine + chn) + 1)
    sta (chn_pos (start engine + chn) + 1)

run engine note_table = mdo
    let pos = 0x00  -- 0x00 stays 0 (the "real" pointer is y:0x01)
        next = do
            iny
            skip bne (inc (pos + 1))
        run_part nesch ch = mdo
            dec (chn_timer ch)
            skip bne $ mdo
                ldy (chn_pos ch)
                pos + 1 *<-* chn_pos ch + 1
                read_note <- here
                ldayp pos
                bmi special
                note <- startof$ mdo
                    asla
                    tax
                    ldax note_table
                    sta (NES.channel_low nesch)
                    ldax (start note_table + 1)
                    sta (NES.channel_high nesch)
                    next
                    ldayp pos
                    sta (chn_timer ch)
                    next
                    jmp done_sound
                special <- startof$ mdo
                    next
                    cmpi repeat_code >> beq do_repeat
                    cmpi set_env_code >> beq do_set_env
                    do_repeat <- startof$ mdo
                        chn_program ch *->* pos
                        chn_program ch + 1 *->* pos + 1
                        jmp read_note
                    do_set_env <- startof$ mdo
                        ldayp pos
                        sta (NES.channel_env nesch)
                        next
                        jmp read_note
                    nothing
                done_sound <- here
                sty (chn_pos ch)
                pos + 1 *->* chn_pos ch + 1
    run_part NES.pulse1 (start engine + square1)
    run_part NES.pulse2 (start engine + square2)


repeat_code : set_env_code : _ = [0x80..] :: [Word8]

repeat = [repeat_code]
set_env val = [set_env_code, val]
