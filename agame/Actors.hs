
{-# LANGUAGE RecursiveDo #-}

module Actors where

import Assembler
import ASM
import ASM6502
import NES
import NES.Reservations
import Data.Word
import Text.Printf

data Actors = Actors {
    amount :: Word8,
    t_models :: Section6502 (),
    xs :: Section Word8 (),
    ys :: Section Word8 (),
    ts :: Section Word8 (),
    fs :: Section Word8 (),
    camera :: Section Word8 (),
    sprites_left :: Section Word8 (),
    start_draw' :: ASM6502 (Section6502 ()),
    draw_actors' :: ASM6502 (Section6502 ()),
    finish_draw' :: ASM6502 (Section6502 ())
}

actors' :: Word8 -> Section6502 () -> ASM6502 Actors
actors' amount t_models = do
    s <- sect "actors" $ do
        fail_assembler_if (size t_models /= fromIntegral amount * 2)
            (printf "Second argument given to actors' (t_models) was the wrong size (0x%x /= 0x%x)"
                    (size t_models) (amount * 2))
        xs <- resz amount
        ys <- resz amount
        ts <- resz amount
        fs <- resz amount
        camera <- resz 2
        sprites_left <- resz 1
        return Actors {
            amount = amount,
            t_models = t_models,
            xs=xs, ys=ys, ts=ts, fs=fs, camera=camera,
            sprites_left=sprites_left,
            start_draw' = sect "Actors.start_draw" $ do
                ldai 0x40
                sta sprites_left
                ldai 0x00
                sta NES.oamaddr
            ,
            draw_actors' = sect "Actors.draw_actors" $ do
                let modelp = 0x00
                    model_size = 0x02
                repfor (ldxi 0) (inx >> cpxi amount >>. bne) $ do
                    ldax ts
                    asla
                    tay
                    lday t_models
                    sta modelp
                    lday (t_models + 1)
                    sta (modelp + 1)
                    ldyi 0x00
                    ldayp modelp
                    sta model_size
                    iny
                    rep (cpy model_size >>. bcc) $ do
                        ldayp modelp
                        clc >> adcx ys
                        sub (camera + 0)
                        sta NES.oamdata
                        iny
                        ldayp modelp
                        sta NES.oamdata
                        iny
                        ldayp modelp
                        andx fs
                        sta NES.oamdata
                        iny
                        ldayp modelp
                        clc >> adcx xs
                        sub (camera + 1)
                        sta NES.oamdata
                        iny
                        dec sprites_left
            ,
            finish_draw' = sect "Actors.finish_draw" $ mdo
                ldx sprites_left
                beq ギリギリ
                lda 0xfe
                rep (dex >>. bne) $ do
                    sta NES.oamdata
                    sta NES.oamdata
                    sta NES.oamdata
                    sta NES.oamdata
                ギリギリ <- here
                nothing
        }
    return (section_return s)


