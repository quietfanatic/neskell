
{-# LANGUAGE RecursiveDo #-}

module NES.ASoundEngine where

import ASM
import ASM6502
import NES

chn_program = (+ 0)
chn_pos = (+ 2)
chn_timer = (+ 4)

sndstate_square1 = (+ 0)
sndstate_square2 = (+ 8)

init_sound_engine sndstate program1 program2 default_env = mdo
    let init_part nesch ch program = do
        low program ->* (chn_program ch)
        high program ->* (chn_program ch) + 1
        0x00 ->* chn_pos ch
        0x04 ->* chn_timer ch
        default_env ->* channel_env nesch
    init_part NES.pulse1 (sndstate_square1 sndstate) program1
    init_part NES.pulse2 (sndstate_square2 sndstate) program2
    

sound_engine sndstate note_table = mdo
    let run_part nesch ch default_env = mdo
        let timer = chn_timer ch
            pos = chn_pos ch
            program = chn_program ch
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
                default_env ->* env
                iny
                jmp done_sound
            rest <- startof$ mdo
                0x30 ->* env
                iny
                jmp done_sound
            special <- startof$ mdo
                iny
                ldayp program
                beq repeat
                unknown <- startof$ mdo
                    iny
                    jmp read_note
                repeat <- startof$ mdo
                    iny
                    sta pos
                    jmp read_note
                nothing
            done_sound <- here
            sty pos
    run_part NES.pulse1 (sndstate_square1 sndstate) 0x33
    run_part NES.pulse2 (sndstate_square2 sndstate) 0x32

repeat = hex "0000"

