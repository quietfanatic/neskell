
module ASM6502 where

import Data.Word
import Data.Int
import qualified Data.Bits as B
import qualified Data.Sequence as S
import Assembly
import ASM

low :: Integral a => a -> Word8
low = fromIntegral
high :: Integral a => a -> Word8
high x = fromIntegral (B.shiftR (fromIntegral x :: Word16) 8)

op8 :: (Integral x, Show x) => Word8 -> x -> ASM ()
op8 a x = byte a >> Assembly f where
    f start = let
        res = case no_overflow x :: Maybe Word8 of
            Just w8 -> S.singleton w8
            Nothing -> error$ "Overflow error in op8 (" ++ show x ++ ")"
        in (res, succ start, ())

op16 :: (Integral x, Show x) => Word8 -> x -> ASM ()
op16 a x = byte a >> Assembly f where
    f start = let
        res16 = case no_overflow x :: Maybe Word8 of
            Just w8 -> fromIntegral w8
            Nothing -> case no_overflow x :: Maybe Word16 of
                Just w16 -> w16
                Nothing -> error$ "Overflow error in op16 (" ++ show x ++ ")"
        res = S.singleton (fromIntegral res16) S.>< S.singleton (fromIntegral (B.shiftR res16 8))
        in (res, succ (succ start), ())

 -- Unlike op8 and op16, this is strict regarding its argument, because the size of the
 -- resulting code depends on the argument.
op8or16 :: (Integral x, Show x) => Word8 -> Word8 -> x -> ASM ()
op8or16 a b x = case no_overflow x of
    Just w8 -> byte a >> byte w8
    Nothing -> case no_overflow x of
        Just w16 -> byte b >> le16 w16
        Nothing -> fail$ "Overflow error in op8or16 (" ++ show x ++ ")"

rel8 :: Integral a => Word8 -> a -> ASM ()
rel8 b x = byte b >> Assembly f where
    f start = let
        off = fromIntegral x - succ start
        res = case no_overflow off :: Maybe Int8 of
            Just i8 -> S.singleton (fromIntegral i8)
            Nothing -> fail$ "Overflow error in relative calculation (" ++ show off ++ ")"
        in (res, succ start, ())


adci :: (Integral a, Show a) => a -> ASM ()
adci = op8 0x69
adcz :: (Integral a, Show a) => a -> ASM ()
adcz = op8 0x65
adcm :: (Integral a, Show a) => a -> ASM ()
adcm = op16 0x6d
adc :: (Integral a, Show a) => a -> ASM ()
adc = op8or16 0x65 0x6d
adcxz :: (Integral a, Show a) => a -> ASM ()
adcxz = op8 0x75
adcxm :: (Integral a, Show a) => a -> ASM ()
adcxm = op16 0x7d
adcx :: (Integral a, Show a) => a -> ASM ()
adcx = op8or16 0x75 0x7d
adcy :: (Integral a, Show a) => a -> ASM ()
adcy = op16 0x79
adcxp :: (Integral a, Show a) => a -> ASM ()
adcxp = op8 0x61
adcpy :: (Integral a, Show a) => a -> ASM ()
adcpy = op8 0x71

andi :: (Integral a, Show a) => a -> ASM ()
andi = op8 0x29
andz :: (Integral a, Show a) => a -> ASM ()
andz = op8 0x25
andm :: (Integral a, Show a) => a -> ASM ()
andm = op16 0x2d
and :: (Integral a, Show a) => a -> ASM ()
and = op8or16 0x25 0x2d
andxz :: (Integral a, Show a) => a -> ASM ()
andxz = op8 0x35
andxm :: (Integral a, Show a) => a -> ASM ()
andxm = op16 0x3d
andx :: (Integral a, Show a) => a -> ASM ()
andx = op8or16 0x35 0x3d
andy :: (Integral a, Show a) => a -> ASM ()
andy = op16 0x39
andpx :: (Integral a, Show a) => a -> ASM ()
andpx = op8 0x21
andyp :: (Integral a, Show a) => a -> ASM ()
andyp = op8 0x31

asla = byte 0x0a
aslz :: (Integral a, Show a) => a -> ASM ()
aslz = op8 0x06
aslm :: (Integral a, Show a) => a -> ASM ()
aslm = op16 0x0e
asl :: (Integral a, Show a) => a -> ASM ()
asl = op8or16 0x06 0x0e
aslxz :: (Integral a, Show a) => a -> ASM ()
aslxz = op8 0x16
aslxm :: (Integral a, Show a) => a -> ASM ()
aslxm = op16 0x1e
aslx :: (Integral a, Show a) => a -> ASM ()
aslx = op8or16 0x16 0x1e

bcc :: (Integral a, Show a) => a -> ASM ()
bcc = rel8 0x90

bcs :: (Integral a, Show a) => a -> ASM ()
bcs = rel8 0xb0

beq :: (Integral a, Show a) => a -> ASM ()
beq = rel8 0xf0

bitz :: (Integral a, Show a) => a -> ASM ()
bitz = op8 0x24
bitm :: (Integral a, Show a) => a -> ASM ()
bitm = op16 0x2c
bit :: (Integral a, Show a) => a -> ASM ()
bit = op8or16 0x24 0x2c

bmi :: (Integral a, Show a) => a -> ASM ()
bmi = rel8 0x30

bne :: (Integral a, Show a) => a -> ASM ()
bne = rel8 0xd0

bpl :: (Integral a, Show a) => a -> ASM ()
bpl = rel8 0x10

brk = byte 0x00

bvc :: (Integral a, Show a) => a -> ASM ()
bvc = rel8 0x50

bvs :: (Integral a, Show a) => a -> ASM ()
bvs = rel8 0x70

clc = byte 0x18

cld = byte 0xd8

cli = byte 0x58

clv = byte 0xb8

cmpi :: (Integral a, Show a) => a -> ASM ()
cmpi = op8 0xc9

cmpz :: (Integral a, Show a) => a -> ASM ()
cmpz = op8 0xc5
cmpm :: (Integral a, Show a) => a -> ASM ()
cmpm = op16 0xcd
cmp :: (Integral a, Show a) => a -> ASM ()
cmp = op8or16 0xc5 0xcd
cmpxz :: (Integral a, Show a) => a -> ASM ()
cmpxz = op8 0xd5
cmpxm :: (Integral a, Show a) => a -> ASM ()
cmpxm = op16 0xdd
cmpx :: (Integral a, Show a) => a -> ASM ()
cmpx = op8or16 0xd5 0xdd
cmpy :: (Integral a, Show a) => a -> ASM ()
cmpy = op16 0xd9
cmppx :: (Integral a, Show a) => a -> ASM ()
cmppx = op8 0xc1
cmpyp :: (Integral a, Show a) => a -> ASM ()
cmpyp = op8 0xd1

cpxi :: (Integral a, Show a) => a -> ASM ()
cpxi = op8 0xe0
cpxz :: (Integral a, Show a) => a -> ASM ()
cpxz = op8 0xe4
cpxm :: (Integral a, Show a) => a -> ASM ()
cpxm = op16 0xec
cpx :: (Integral a, Show a) => a -> ASM ()
cpx = op8or16 0xe4 0xec

cpyi :: (Integral a, Show a) => a -> ASM ()
cpyi = op8 0xc0
cpyz :: (Integral a, Show a) => a -> ASM ()
cpyz = op8 0xc4
cpym :: (Integral a, Show a) => a -> ASM ()
cpym = op16 0xcc
cpy :: (Integral a, Show a) => a -> ASM ()
cpy = op8or16 0xc4 0xcc

decz :: (Integral a, Show a) => a -> ASM ()
decz = op8 0xc6
decm :: (Integral a, Show a) => a -> ASM ()
decm = op16 0xce
dec :: (Integral a, Show a) => a -> ASM ()
dec = op8or16 0xc6 0xce
decxz :: (Integral a, Show a) => a -> ASM ()
decxz = op8 0xd6
decxm :: (Integral a, Show a) => a -> ASM ()
decxm = op16 0xde
decx :: (Integral a, Show a) => a -> ASM ()
decx = op8or16 0xd6 0xde

dex = byte 0xca

dey = byte 0x88

eori :: (Integral a, Show a) => a -> ASM ()
eori = op8 0x49
eorz :: (Integral a, Show a) => a -> ASM ()
eorz = op8 0x45
eorm :: (Integral a, Show a) => a -> ASM ()
eorm = op16 0x4d
eor :: (Integral a, Show a) => a -> ASM ()
eor = op8or16 0x45 0x4d
eorxz :: (Integral a, Show a) => a -> ASM ()
eorxz = op8 0x55
eorxm :: (Integral a, Show a) => a -> ASM ()
eorxm = op16 0x5d
eorx :: (Integral a, Show a) => a -> ASM ()
eorx = op8or16 0x55 0x5d
eory :: (Integral a, Show a) => a -> ASM ()
eory = op16 0x59
eorpx :: (Integral a, Show a) => a -> ASM ()
eorpx = op8 0x41
eoryp :: (Integral a, Show a) => a -> ASM ()
eoryp = op8 0x51

incz :: (Integral a, Show a) => a -> ASM ()
incz = op8 0xe6
incm :: (Integral a, Show a) => a -> ASM ()
incm = op16 0xee
inc :: (Integral a, Show a) => a -> ASM ()
inc = op8or16 0xe6 0xee
incxz :: (Integral a, Show a) => a -> ASM ()
incxz = op8 0xf6
incxm :: (Integral a, Show a) => a -> ASM ()
incxm = op16 0xfe
incx :: (Integral a, Show a) => a -> ASM ()
incx = op8or16 0xf6 0xfe

inx = byte 0xe8

iny = byte 0xc8

jmp :: (Integral a, Show a) => a -> ASM ()
jmp = op16 0x4c

jmpp :: (Integral a, Show a) => a -> ASM ()
jmpp = op16 0x6c

jsr :: (Integral a, Show a) => a -> ASM ()
jsr = op16 0x20

ldai :: (Integral a, Show a) => a -> ASM ()
ldai = op8 0xa9
ldaz :: (Integral a, Show a) => a -> ASM ()
ldaz = op8 0xa5
ldam :: (Integral a, Show a) => a -> ASM ()
ldam = op16 0xad
lda :: (Integral a, Show a) => a -> ASM ()
lda = op8or16 0xa5 0xad
ldaxz :: (Integral a, Show a) => a -> ASM ()
ldaxz = op8 0xb5
ldaxm :: (Integral a, Show a) => a -> ASM ()
ldaxm = op16 0xbd
ldax :: (Integral a, Show a) => a -> ASM ()
ldax = op8or16 0xb5 0xbd
lday :: (Integral a, Show a) => a -> ASM ()
lday = op16 0xb9
ldapx :: (Integral a, Show a) => a -> ASM ()
ldapx = op8 0xa1
ldayp :: (Integral a, Show a) => a -> ASM ()
ldayp = op8 0xb1

ldxi :: (Integral a, Show a) => a -> ASM ()
ldxi = op8 0xa2
ldxz :: (Integral a, Show a) => a -> ASM ()
ldxz = op8 0xa6
ldxm :: (Integral a, Show a) => a -> ASM ()
ldxm = op16 0xae
ldx :: (Integral a, Show a) => a -> ASM ()
ldx = op8or16 0xa6 0xae
ldxyz :: (Integral a, Show a) => a -> ASM ()
ldxyz = op8 0xb6
ldxym :: (Integral a, Show a) => a -> ASM ()
ldxym = op16 0xbe
ldxy :: (Integral a, Show a) => a -> ASM ()
ldxy = op8or16 0xb6 0xbe

ldyi :: (Integral a, Show a) => a -> ASM ()
ldyi = op8 0xa0
ldyz :: (Integral a, Show a) => a -> ASM ()
ldyz = op8 0xa4
ldym :: (Integral a, Show a) => a -> ASM ()
ldym = op16 0xac
ldy :: (Integral a, Show a) => a -> ASM ()
ldy = op8or16 0xa4 0xac
ldyxz :: (Integral a, Show a) => a -> ASM ()
ldyxz = op8 0xb4
ldyxm :: (Integral a, Show a) => a -> ASM ()
ldyxm = op16 0xbc
ldyx :: (Integral a, Show a) => a -> ASM ()
ldyx = op8or16 0xb4 0xbc

lsra = byte 0x4a
lsrz :: (Integral a, Show a) => a -> ASM ()
lsrz = op8 0x46
lsrm :: (Integral a, Show a) => a -> ASM ()
lsrm = op16 0x4e
lsr :: (Integral a, Show a) => a -> ASM ()
lsr = op8or16 0x46 0x4e
lsrxz :: (Integral a, Show a) => a -> ASM ()
lsrxz = op8 0x56
lsrxm :: (Integral a, Show a) => a -> ASM ()
lsrxm = op16 0x5e
lsrx :: (Integral a, Show a) => a -> ASM ()
lsrx = op8or16 0x56 0x5e

nop = byte 0xea

orai :: (Integral a, Show a) => a -> ASM ()
orai = op8 0x09
oraz :: (Integral a, Show a) => a -> ASM ()
oraz = op8 0x05
oram :: (Integral a, Show a) => a -> ASM ()
oram = op16 0x0d
ora :: (Integral a, Show a) => a -> ASM ()
ora = op8or16 0x05 0x0d
oraxz :: (Integral a, Show a) => a -> ASM ()
oraxz = op8 0x15
oraxm :: (Integral a, Show a) => a -> ASM ()
oraxm = op16 0x1d
orax :: (Integral a, Show a) => a -> ASM ()
orax = op8or16 0x15 0x1d
oray :: (Integral a, Show a) => a -> ASM ()
oray = op16 0x19
orapx :: (Integral a, Show a) => a -> ASM ()
orapx = op8 0x01
orayp :: (Integral a, Show a) => a -> ASM ()
orayp = op8 0x11

pha = byte 0x48

php = byte 0x08

pla = byte 0x68

plp = byte 0x28

rola = byte 0x2a
rolz :: (Integral a, Show a) => a -> ASM ()
rolz = op8 0x26
rolm :: (Integral a, Show a) => a -> ASM ()
rolm = op16 0x2e
rol :: (Integral a, Show a) => a -> ASM ()
rol = op8or16 0x26 0x2e
rolxz :: (Integral a, Show a) => a -> ASM ()
rolxz = op8 0x36
rolxm :: (Integral a, Show a) => a -> ASM ()
rolxm = op16 0x3e
rolx :: (Integral a, Show a) => a -> ASM ()
rolx = op8or16 0x36 0x3e

rora = byte 0x6a
rorz :: (Integral a, Show a) => a -> ASM ()
rorz = op8 0x66
rorm :: (Integral a, Show a) => a -> ASM ()
rorm = op16 0x6e
ror :: (Integral a, Show a) => a -> ASM ()
ror = op8or16 0x66 0x6e
rorxz :: (Integral a, Show a) => a -> ASM ()
rorxz = op8 0x76
rorxm :: (Integral a, Show a) => a -> ASM ()
rorxm = op16 0x7e
rorx :: (Integral a, Show a) => a -> ASM ()
rorx = op8or16 0x76 0x7e

rti = byte 0x40

rts = byte 0x60

sbci :: (Integral a, Show a) => a -> ASM ()
sbci = op8 0xe9
sbcz :: (Integral a, Show a) => a -> ASM ()
sbcz = op8 0xe5
sbcm :: (Integral a, Show a) => a -> ASM ()
sbcm = op16 0xed
sbc :: (Integral a, Show a) => a -> ASM ()
sbc = op8or16 0xe5 0xed
sbcxz :: (Integral a, Show a) => a -> ASM ()
sbcxz = op8 0xf5
sbcxm :: (Integral a, Show a) => a -> ASM ()
sbcxm = op16 0xfd
sbcx :: (Integral a, Show a) => a -> ASM ()
sbcx = op8or16 0xf5 0xfd
sbcy :: (Integral a, Show a) => a -> ASM ()
sbcy = op16 0xf9
sbcpx :: (Integral a, Show a) => a -> ASM ()
sbcpx = op8 0xe1
sbcyp :: (Integral a, Show a) => a -> ASM ()
sbcyp = op8 0xf1

sec = byte 0x38

sed = byte 0xf8

sei = byte 0x78

staz :: (Integral a, Show a) => a -> ASM ()
staz = op8 0x85
stam :: (Integral a, Show a) => a -> ASM ()
stam = op16 0x8d
sta :: (Integral a, Show a) => a -> ASM ()
sta = op8or16 0x85 0x8d
staxz :: (Integral a, Show a) => a -> ASM ()
staxz = op8 0x95
staxm :: (Integral a, Show a) => a -> ASM ()
staxm = op16 0x9d
stax :: (Integral a, Show a) => a -> ASM ()
stax = op8or16 0x95 0x9d
stay :: (Integral a, Show a) => a -> ASM ()
stay = op16 0x99
stapx :: (Integral a, Show a) => a -> ASM ()
stapx = op8 0x81
stayp :: (Integral a, Show a) => a -> ASM ()
stayp = op8 0x91

stxz :: (Integral a, Show a) => a -> ASM ()
stxz = op8 0x86
stxm :: (Integral a, Show a) => a -> ASM ()
stxm = op16 0x8e
stx :: (Integral a, Show a) => a -> ASM ()
stx = op8or16 0x86 0x8e
stxy :: (Integral a, Show a) => a -> ASM ()
stxy = op8 0x8e

styz :: (Integral a, Show a) => a -> ASM ()
styz = op8 0x84
stym :: (Integral a, Show a) => a -> ASM ()
stym = op16 0x8c
sty :: (Integral a, Show a) => a -> ASM ()
sty = op8or16 0x84 0x8c
styx :: (Integral a, Show a) => a -> ASM ()
styx = op8 0x94

tax = byte 0xaa

tay = byte 0xa8

tsx = byte 0xba

txa = byte 0x8a

txs = byte 0x9a

tya = byte 0x98


infix 2 ->*
infix 2 *->*
val ->* mem = ldai val >> sta mem
a *->* b = lda a >> sta b

