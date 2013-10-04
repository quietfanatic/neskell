module NES.Header001 (
    reset', writeA', writeA0', write'
) where
import Data.Word
import Assembler
import ASM6502
import NES

 -- Note: this only resets the temporary transfer register and bits
 --  2-3 of register 0x8000
reset' = sect "NES.Mapper001.reset'" $ do
    ldai 0x80
    sta 0x8000

writeA' :: Word16 -> ASM6502 (Section6502 ())
writeA' reg = sect "NES.Mapper001.writeA'" $ do
    sta reg
    sequence_ $ replicate 4 (lsra >> sta reg)

writeA0' :: Word16 -> ASM6502 (Section6502 ())
writeA0' reg = sect "NES.Mapper001.writeA0'" $ do
    sequence_ $ replicate 5 (sta reg)

write' :: Word16 -> Word8 -> ASM6502 (Section6502 ())
write' reg val = sect "NES.Mapper001.write'" $ do
    ldai val
    if val == 0 then writeA0' reg else writeA' reg
    return ()




