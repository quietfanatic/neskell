
{-# LANGUAGE RecursiveDo, DeriveDataTypeable #-}

 -- Please import this qualified.
module NES.ASoundEngine (
    ASoundEngine, a_sound_engine, initialize, set_stream, run,
    note, delay, loop, repeat, set_env, call
) where

import Prelude hiding (repeat)
import Data.Word
import Data.Typeable
import Assembler
import ASM
import ASM6502
import NES hiding (initialize)
import NES.Reservations
import Text.Printf

data ASoundEngine = ASoundEngine (Section6502 ()) (Section6502 ())
engine_note_table (ASoundEngine x _) = x
engine_state (ASoundEngine _ x) = x
 -- Data is laid out in fours to match the NES channels
 -- 11112222ttttnnnn11--22--tt--nn--
engine_position = (+ 0x00) . section_start . engine_state
engine_timer = (+ 0x02) . section_start . engine_state
engine_reps = (+ 0x10) . section_start . engine_state  -- Takes up two slots for two loop units
engine_size = 0x20 :: Word16

 -- Takes a note table as input.
a_sound_engine :: Section6502 () -> ASM6502 ASoundEngine
a_sound_engine note_table = do
    state <- res engine_size
    return $ ASoundEngine note_table state

initialize :: ASoundEngine -> ASM6502 (Section6502 ())
initialize engine = area "NES.ASoundEngine.init" $ mdo
    let init_part :: Word16 -> ASM6502 ()
        init_part chn = do
         -- We're assuming memory has been zeroed out.
        0x01 ->* engine_timer engine + chn
    init_part NES.pulse1
    init_part NES.pulse2
    init_part NES.triangle

set_stream engine chn stream = area "NES.ASoundEngine.set_stream" $ mdo
    ldai (low stream)
    sta (engine_position engine + chn)
    ldai (high stream)
    sta (engine_position engine + chn + 1)

run :: ASoundEngine -> ASM6502 (Section6502 ())
run engine = area "NES.ASoundEngine.run" $ mdo
     -- X is always the channel offset (0, 4, 8, or c)
     -- Y is either the low end of pos or the note index.
    let note_table = engine_note_table engine
        position = engine_position engine
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
    read_commands <- mdo
        ldax (position + 1)
        beq just_wait
        decx timer
        bne just_wait
        read_commands <- mdo
            ldyx position
            sta (pos + 1)
            read_commands <- section$ mdo
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
                        jmp read_commands
                    next
                    stax timer
                    jmp (end read_commands)
                lday command_table  -- Do a special comand
                sta command_ptr
                lday (command_table + 1)
                sta (command_ptr + 1)
                ldy tmpy
                jmpp command_ptr
            tya
            stax position
            lda (pos + 1) >> stax (position + 1)
            return read_commands
        just_wait <- here
        return read_commands
    inx >> inx >> inx >> inx
    skip (cpxi 0x0c >>. beq) (jmp run_one)
    jmp (end command_table)
    command_table <- section$ mdo
        le16 command_loopa
        le16 command_loopb
        le16 command_delay
        le16 command_set_env
        le16 command_call
        command_loopb <- section$ mdo
            stx tmpy
            inx
            jmp command_loop_start
        command_loopa <- here
        command_loop_start <- mdo
            stx tmpy
            command_loop_start <- here
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
                ldx tmpy
                jmp read_commands
            do_goto <- section$ mdo
                ldx tmpy
                ldayp pos  -- reps is non-zero or loop is infinite
                sta tmpy
                next
                ldayp pos
                sta (pos + 1)
                ldy tmpy
                jmp read_commands
            return command_loop_start
        command_delay <- section$ mdo
            ldayp pos >> stax timer
            next
            jmp (end read_commands)
        command_set_env <- section$ mdo
            ldayp pos >> stax NES.chn_env
            next
            jmp read_commands
        command_call <- section$ mdo
            ldayp pos >> sta command_ptr
            next
            ldayp pos >> sta (command_ptr + 1)
            next
            jsr call_sub
            jmp read_commands
        call_sub <- section$ mdo
            jmpp command_ptr
        nothing
    nothing

loopa_code : loopb_code : delay_code : set_env_code : call_code : _ = [0x80..] :: [Word8]

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

newtype LoopCount = LoopCount Int deriving (Typeable)

loop_code spot 0 = loopa_code
loop_code spot 1 = loopb_code
loop_code spot _ = error$ printf "Too many nested loops in music stream at 0x%x" spot

loop :: Word8 -> ASM6502 () -> ASM6502 ()
loop times code = do
    begin <- here
    LoopCount c <- get_annotation_default (LoopCount 0)
    set_annotation (Just (LoopCount (c + 1)))
    code
    set_annotation (Just (LoopCount c))
    byte (loop_code begin c)
    byte times
    le16 begin

repeat :: ASM6502 () -> ASM6502 ()
repeat code = do
    begin <- here
    code
    byte loopa_code
    byte 0
    le16 begin

set_env :: Word8 -> ASM6502 ()
set_env val = byte set_env_code >> byte val

call :: Integral a => a -> ASM6502 ()
call = op16 "NES.ASoundEngine.call" call_code
