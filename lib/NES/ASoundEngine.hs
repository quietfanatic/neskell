
{-# LANGUAGE RecursiveDo #-}

 -- Please import this qualified.
module NES.ASoundEngine where

import Data.Word
import Assembly
import ASM
import ASM6502
import NES
import Text.Printf

datasize = 0x20

 -- Data is laid out in fours to match the NES channels
 -- 11112222ttttnnnn11--22--tt--nn--
engine_position (Allocation x _) = x + 0x00
engine_timer (Allocation x _) = x + 0x02
engine_reps (Allocation x _) = x + 0x10

validate (Allocation _ s) cont = if s == datasize
    then cont
    else error$ "Sound engine was given an allocation of the wrong size (" ++ show s ++ " /= " ++ show datasize ++ ")"

init :: Allocation Word16 -> ASM6502 ()
init engine = validate engine $ mdo
    let init_part :: Word16 -> ASM6502 ()
        init_part chn = do
         -- We're assuming memory has been zeroed out.
        0x01 ->* engine_timer engine + chn
    init_part NES.pulse1
    init_part NES.pulse2
    init_part NES.triangle

set_stream engine chn stream = validate engine $ do
    ldai (low stream)
    sta (engine_position engine + chn)
    ldai (high stream)
    sta (engine_position engine + chn + 1)

run :: HasArea nt => Allocation Word16 -> nt Word16 -> ASM6502 ()
run engine note_table = mdo
     -- X is always the channel offset (0, 4, 8, or c)
     -- Y is either the low end of pos or the note index.
    let position = engine_position engine
        timer = engine_timer engine
        reps = engine_reps engine
        pos = 0x00  -- 0x00 stays 0 (the real pointer is y:0x01)
        tmpy = 0x02
        command_ptr = 0x03
        tmppos = 0x05
        next = do
            iny
            skip bne (inc (pos + 1))
    ldxi 0x00
    run_one <- here
    (read_command, done_reading) <- mdo
        ldax (position + 1)
        beq just_wait
        decx timer
        bne just_wait
        (read_command, done_reading) <- mdo
            ldyx position
            sta (pos + 1)
            (read_command, done_reading) <- startend$ mdo
                ldayp pos
                next
                sty tmpy
                asla
                tay
                skip bcs $ mdo  -- play a note
                    lday (start note_table)
                    stax NES.chn_low
                    lday (start note_table + 1)
                    orai 0xf8
                    stax NES.chn_high
                    ldy tmpy
                    ldayp pos  -- Also read a delay.
                    skip bne $ mdo
                        next
                        jmp read_command
                    next
                    stax timer
                    jmp done_reading
                lday command_table  -- Do a special comand
                sta command_ptr
                lday (command_table + 1)
                sta (command_ptr + 1)
                ldy tmpy
                jmpp command_ptr
            tya
            stax position
            lda (pos + 1) >> stax (position + 1)
            return (read_command, done_reading)
        just_wait <- here
        return (read_command, done_reading)
    inx >> inx >> inx >> inx
    skip (cpxi 0x0c >>. beq) (jmp run_one)
    jmp done
    (command_table, done) <- startend$ mdo
        le16 command_loop
        le16 command_delay
        le16 command_set_env
        command_loop <- startof$ mdo
            ldax reps
            skip bne$ mdo
                ldayp pos  -- reps is zero; start loop
                skip bne $ mdo
                    next  -- If the program says zero reps it means infinite
                    jmp do_goto
                stax reps
            next
            decx reps
            skip bne $ mdo
                next
                next
                jmp read_command
            do_goto <- startof$ mdo
                ldayp pos  -- reps is non-zero or loop is infinite
                sta tmpy
                next
                ldayp pos
                sta (pos + 1)
                ldy tmpy
                jmp read_command
            nothing
        command_delay <- startof$ mdo
            ldayp pos >> stax timer
            next
            jmp done_reading
        command_set_env <- startof$ mdo
            ldayp pos >> stax NES.chn_env
            next
            jmp read_command
        nothing
    nothing

loop_code : delay_code : set_env_code : _ = [0x80..] :: [Word8]

delaybyte d = if d <= 0xff
    then byte (fromIntegral d)
    else byte 0xff >> delay (d - 0xff)

note n d = do
    pos <- here
    if n <= 0x7f
        then byte (fromIntegral n) >> delaybyte d
        else fail$ printf "Note value is too large (0x%x > 0x7f) at 0x%x" n pos

delay :: Word8 -> ASM6502 ()
delay d = byte delay_code >> delaybyte d

loop :: Word8 -> ASM6502 () -> ASM6502 ()
loop times code = do
    begin <- here
    code
    byte loop_code
    byte times
    le16 begin

set_env :: Word8 -> ASM6502 ()
set_env val = byte set_env_code >> byte val
