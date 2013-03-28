
{-# LANGUAGE RecursiveDo #-}

module NES.ASoundEngine where

import ASM
import ASM6502
import NES

channel_program = (+ 0)
channel_pos = (+ 2)
channel_timer = (+ 4)

init_sound_engine nesch ch program default_env = mdo
    low program ->* (channel_program ch)
    high program ->* (channel_program ch) + 1
    0x00 ->* channel_pos ch
    0x04 ->* channel_timer ch
    0x30 ->* channel_env square1_env  -- 00110000

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

repeat = hex "0000"

