
{-# LANGUAGE RecursiveDo #-}

 -- Please import this qualified.
module NES.ASoundEngine where

import Data.Word
import Assembly
import ASM
import ASM6502
import NES

datasize = 0x20

 -- Data is laid out in fours to match the NES channels
 -- 11112222ttttnnnn11--22--tt--nn--
position (Allocation x _) = x + 0x00
timer (Allocation x _) = x + 0x02
repeat_counter (Allocation x _) = x + 0x10

validate (Allocation _ s) cont = if s == datasize
    then cont
    else error$ "Sound engine was given an allocation of the wrong size (" ++ show s ++ " /= " ++ show datasize ++ ")"

init :: Allocation Word16 -> ASM6502 ()
init engine = validate engine $ mdo
    let init_part :: Word16 -> ASM6502 ()
        init_part chn = do
         -- We're assuming memory has been zeroed out.
        0x01 ->* timer engine + chn
    init_part NES.pulse1
    init_part NES.pulse2
    init_part NES.triangle

set_stream engine chn stream = validate engine $ do
    ldai (low stream)
    sta (position engine + chn)
    ldai (high stream)
    sta (position engine + chn + 1)

run :: HasArea nt => Allocation Word16 -> nt Word16 -> ASM6502 ()
run engine note_table = mdo
     -- X is always the channel offset (0, 4, 8, or c)
     -- Y is either the low end of pos or the note index.
    let eposition = position engine
        etimer = timer engine
        reps = repeat_counter engine
        pos = 0x00  -- 0x00 stays 0 (the real pointer is y:0x01)
        tmpy = 0x02
        next = do
            iny
            skip bne (inc (pos + 1))
    ldxi 0x00
    run_one <- startof$ mdo
        ldax (eposition + 1)
        skip bne (jmp just_wait)
        decx etimer
        skip beq (jmp just_wait)
        just_wait <- endof$ mdo
            ldyx eposition
            sta (pos + 1)
            read_note <- here
            ldayp pos
            bmi special
            note <- startof$ mdo
                sty tmpy
                asla
                tay
                lday (start note_table)
                stax NES.chn_low
                lday (start note_table + 1)
                orai 0xf8
                stax NES.chn_high
                ldy tmpy
                next
                ldayp pos >> stax etimer
                next
                jmp done_sound
            special <- startof$ mdo
                next
                cmpi loop_code >> beq do_loop
                cmpi set_env_code >> beq do_set_env
                do_loop <- startof$ mdo
                    ldax reps
                    skip bne$ mdo
                        ldayp pos  -- reps is zero; start loop
                        beq infiloop  -- If the program says zero reps it means infinite
                        stax reps
                    next
                    decx reps >> beq skip_goto
                    (do_goto, skip_goto) <- startend$ mdo
                        tya  -- reps is non-zero or loop is infitie
                        sec
                        sbcyp pos >> skip bcs (dec (pos + 1))
                        tay
                    next
                    jmp read_note
                    infiloop <- startof$ mdo
                        next
                        jmp do_goto
                    nothing
                do_set_env <- startof$ mdo
                    ldayp pos >> stax NES.chn_env
                    next
                    jmp read_note
                nothing
            done_sound <- here
            tya
            stax eposition
            lda (pos + 1) >> stax (eposition + 1)
        nothing
    inx >> inx >> inx >> inx
    skip (cpxi 0x0c >>. beq) (jmp run_one)

loop_code : set_env_code : _ = [0x80..] :: [Word8]

loop :: Word8 -> ASM6502 () -> ASM6502 ()
loop times code = do
    s <- sizeof code
    byte loop_code
    byte times
    case no_overflow (s + 3) of
        Just w8 -> byte w8
        Nothing -> fail$ "Loop is too large.  This restriction should be lifted soon."
set_env :: Word8 -> ASM6502 ()
set_env val = byte set_env_code >> byte val
