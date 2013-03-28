
module ASM6502 where

import Prelude hiding (and)
import qualified Prelude (and)
import Data.Word
import Data.Int
import qualified Data.Bits as B
import qualified Data.Sequence as S
import Assembly
import ASM

type ASM6502 a = ASM Word16 a

low :: Integral a => a -> Word8
low = fromIntegral
high :: Integral a => a -> Word8
high x = fromIntegral (B.shiftR (fromIntegral x :: Word16) 8)

op8 :: (Integral x, Show x) => Word8 -> x -> ASM6502 ()
op8 a x = byte a >> Assembly f where
    f start = let
        res = case no_overflow x :: Maybe Word8 of
            Just w8 -> S.singleton w8
            Nothing -> error$ "Overflow error in op8 (" ++ show x ++ ")"
        in (res, succ start, ())

op16 :: (Integral x, Show x) => Word8 -> x -> ASM6502 ()
op16 a x = byte a >> Assembly f where
    f start = let
        res16 = case no_overflow x :: Maybe Word8 of
            Just w8 -> fromIntegral w8
            Nothing -> case no_overflow x :: Maybe Word16 of
                Just w16 -> w16
                Nothing -> error$ "Overflow error in op16 (" ++ show x ++ ")"
        res = S.singleton (fromIntegral res16) S.>< S.singleton (fromIntegral (B.shiftR res16 8))
        in (res, succ (succ start), ())

 -- case (maxBound of argument type) of
 --    8bit -> generate op8
 --    16bit -> generate op16
 --    other -> generate op8 or op16 depending on argument's value
 -- This last case will lead to an infinite loop if the argument
 --  depends on the size of the generated op.  Fortunately, arguments with
 --  that dependency are likely to be ASM labels, which are 16bit words.
op8or16 :: (Integral x, Show x, Bounded x) => Word8 -> Word8 -> x -> ASM6502 ()
op8or16 = op8or16' maxBound

op8or16' :: (Integral x, Show x, Bounded x) => x -> Word8 -> Word8 -> x -> ASM6502 ()
op8or16' max a b x = case toInteger max of
    255 -> byte a >> byte (fromIntegral x)
    127 -> byte a >> byte (fromIntegral x)
    65535 -> byte b >> le16 (fromIntegral x)
    32767 -> byte b >> le16 (fromIntegral x)
    _ -> case no_overflow x of
        Just w8 -> byte a >> byte w8
        Nothing -> case no_overflow x of
            Just w16 -> byte b >> le16 w16
            Nothing -> fail$ "Overflow error in op8or16 (" ++ show x ++ ")"

 -- HACK HACK HACK
instance Bounded Integer where
    maxBound = -1
    minBound = error$ "I'm sorry!  I had to define Bounded Integer to make op8or16 work!  Wait, why are you trying to get minBound :: Integer anyway?"


rel8 :: Integral a => Word8 -> a -> ASM6502 ()
rel8 b x = byte b >> Assembly f where
    f start = let
        off = fromIntegral x - fromIntegral (succ start)
        res = case no_overflow off :: Maybe Int8 of
            Just i8 -> S.singleton (fromIntegral i8)
            Nothing -> fail$ "Overflow error in relative calculation (" ++ show off ++ ")"
        in (res, succ start, ())

adci :: (Integral a, Show a) => a -> ASM6502 ()
adci = op8 0x69
adcz :: (Integral a, Show a) => a -> ASM6502 ()
adcz = op8 0x65
adcm :: (Integral a, Show a) => a -> ASM6502 ()
adcm = op16 0x6d
adc :: (Integral a, Show a, Bounded a) => a -> ASM6502 ()
adc = op8or16 0x65 0x6d
adcxz :: (Integral a, Show a) => a -> ASM6502 ()
adcxz = op8 0x75
adcxm :: (Integral a, Show a) => a -> ASM6502 ()
adcxm = op16 0x7d
adcx :: (Integral a, Show a, Bounded a) => a -> ASM6502 ()
adcx = op8or16 0x75 0x7d
adcy :: (Integral a, Show a) => a -> ASM6502 ()
adcy = op16 0x79
adcxp :: (Integral a, Show a) => a -> ASM6502 ()
adcxp = op8 0x61
adcpy :: (Integral a, Show a) => a -> ASM6502 ()
adcpy = op8 0x71

andi :: (Integral a, Show a) => a -> ASM6502 ()
andi = op8 0x29
andz :: (Integral a, Show a) => a -> ASM6502 ()
andz = op8 0x25
andm :: (Integral a, Show a) => a -> ASM6502 ()
andm = op16 0x2d
and :: (Integral a, Show a, Bounded a) => a -> ASM6502 ()
and = op8or16 0x25 0x2d
andxz :: (Integral a, Show a) => a -> ASM6502 ()
andxz = op8 0x35
andxm :: (Integral a, Show a) => a -> ASM6502 ()
andxm = op16 0x3d
andx :: (Integral a, Show a, Bounded a) => a -> ASM6502 ()
andx = op8or16 0x35 0x3d
andy :: (Integral a, Show a) => a -> ASM6502 ()
andy = op16 0x39
andpx :: (Integral a, Show a) => a -> ASM6502 ()
andpx = op8 0x21
andyp :: (Integral a, Show a) => a -> ASM6502 ()
andyp = op8 0x31

asla :: ASM6502 ()
asla = byte 0x0a
aslz :: (Integral a, Show a) => a -> ASM6502 ()
aslz = op8 0x06
aslm :: (Integral a, Show a) => a -> ASM6502 ()
aslm = op16 0x0e
asl :: (Integral a, Show a, Bounded a) => a -> ASM6502 ()
asl = op8or16 0x06 0x0e
aslxz :: (Integral a, Show a) => a -> ASM6502 ()
aslxz = op8 0x16
aslxm :: (Integral a, Show a) => a -> ASM6502 ()
aslxm = op16 0x1e
aslx :: (Integral a, Show a, Bounded a) => a -> ASM6502 ()
aslx = op8or16 0x16 0x1e

bcc :: (Integral a, Show a) => a -> ASM6502 ()
bcc = rel8 0x90

bcs :: (Integral a, Show a) => a -> ASM6502 ()
bcs = rel8 0xb0

beq :: (Integral a, Show a) => a -> ASM6502 ()
beq = rel8 0xf0

bitz :: (Integral a, Show a) => a -> ASM6502 ()
bitz = op8 0x24
bitm :: (Integral a, Show a) => a -> ASM6502 ()
bitm = op16 0x2c
bit :: (Integral a, Show a, Bounded a) => a -> ASM6502 ()
bit = op8or16 0x24 0x2c

bmi :: (Integral a, Show a) => a -> ASM6502 ()
bmi = rel8 0x30

bne :: (Integral a, Show a) => a -> ASM6502 ()
bne = rel8 0xd0

bpl :: (Integral a, Show a) => a -> ASM6502 ()
bpl = rel8 0x10

brk :: ASM6502 ()
brk = byte 0x00

bvc :: (Integral a, Show a) => a -> ASM6502 ()
bvc = rel8 0x50

bvs :: (Integral a, Show a) => a -> ASM6502 ()
bvs = rel8 0x70

clc :: ASM6502 ()
clc = byte 0x18

cld :: ASM6502 ()
cld = byte 0xd8

cli :: ASM6502 ()
cli = byte 0x58

clv :: ASM6502 ()
clv = byte 0xb8

cmpi :: (Integral a, Show a) => a -> ASM6502 ()
cmpi = op8 0xc9

cmpz :: (Integral a, Show a) => a -> ASM6502 ()
cmpz = op8 0xc5
cmpm :: (Integral a, Show a) => a -> ASM6502 ()
cmpm = op16 0xcd
cmp :: (Integral a, Show a, Bounded a) => a -> ASM6502 ()
cmp = op8or16 0xc5 0xcd
cmpxz :: (Integral a, Show a) => a -> ASM6502 ()
cmpxz = op8 0xd5
cmpxm :: (Integral a, Show a) => a -> ASM6502 ()
cmpxm = op16 0xdd
cmpx :: (Integral a, Show a, Bounded a) => a -> ASM6502 ()
cmpx = op8or16 0xd5 0xdd
cmpy :: (Integral a, Show a) => a -> ASM6502 ()
cmpy = op16 0xd9
cmppx :: (Integral a, Show a) => a -> ASM6502 ()
cmppx = op8 0xc1
cmpyp :: (Integral a, Show a) => a -> ASM6502 ()
cmpyp = op8 0xd1

cpxi :: (Integral a, Show a) => a -> ASM6502 ()
cpxi = op8 0xe0
cpxz :: (Integral a, Show a) => a -> ASM6502 ()
cpxz = op8 0xe4
cpxm :: (Integral a, Show a) => a -> ASM6502 ()
cpxm = op16 0xec
cpx :: (Integral a, Show a, Bounded a) => a -> ASM6502 ()
cpx = op8or16 0xe4 0xec

cpyi :: (Integral a, Show a) => a -> ASM6502 ()
cpyi = op8 0xc0
cpyz :: (Integral a, Show a) => a -> ASM6502 ()
cpyz = op8 0xc4
cpym :: (Integral a, Show a) => a -> ASM6502 ()
cpym = op16 0xcc
cpy :: (Integral a, Show a, Bounded a) => a -> ASM6502 ()
cpy = op8or16 0xc4 0xcc

decz :: (Integral a, Show a) => a -> ASM6502 ()
decz = op8 0xc6
decm :: (Integral a, Show a) => a -> ASM6502 ()
decm = op16 0xce
dec :: (Integral a, Show a, Bounded a) => a -> ASM6502 ()
dec = op8or16 0xc6 0xce
decxz :: (Integral a, Show a) => a -> ASM6502 ()
decxz = op8 0xd6
decxm :: (Integral a, Show a) => a -> ASM6502 ()
decxm = op16 0xde
decx :: (Integral a, Show a, Bounded a) => a -> ASM6502 ()
decx = op8or16 0xd6 0xde

dex :: ASM6502 ()
dex = byte 0xca

dey :: ASM6502 ()
dey = byte 0x88

eori :: (Integral a, Show a) => a -> ASM6502 ()
eori = op8 0x49
eorz :: (Integral a, Show a) => a -> ASM6502 ()
eorz = op8 0x45
eorm :: (Integral a, Show a) => a -> ASM6502 ()
eorm = op16 0x4d
eor :: (Integral a, Show a, Bounded a) => a -> ASM6502 ()
eor = op8or16 0x45 0x4d
eorxz :: (Integral a, Show a) => a -> ASM6502 ()
eorxz = op8 0x55
eorxm :: (Integral a, Show a) => a -> ASM6502 ()
eorxm = op16 0x5d
eorx :: (Integral a, Show a, Bounded a) => a -> ASM6502 ()
eorx = op8or16 0x55 0x5d
eory :: (Integral a, Show a) => a -> ASM6502 ()
eory = op16 0x59
eorpx :: (Integral a, Show a) => a -> ASM6502 ()
eorpx = op8 0x41
eoryp :: (Integral a, Show a) => a -> ASM6502 ()
eoryp = op8 0x51

incz :: (Integral a, Show a) => a -> ASM6502 ()
incz = op8 0xe6
incm :: (Integral a, Show a) => a -> ASM6502 ()
incm = op16 0xee
inc :: (Integral a, Show a, Bounded a) => a -> ASM6502 ()
inc = op8or16 0xe6 0xee
incxz :: (Integral a, Show a) => a -> ASM6502 ()
incxz = op8 0xf6
incxm :: (Integral a, Show a) => a -> ASM6502 ()
incxm = op16 0xfe
incx :: (Integral a, Show a, Bounded a) => a -> ASM6502 ()
incx = op8or16 0xf6 0xfe

inx :: ASM6502 ()
inx = byte 0xe8

iny :: ASM6502 ()
iny = byte 0xc8

jmp :: (Integral a, Show a) => a -> ASM6502 ()
jmp = op16 0x4c

jmpp :: (Integral a, Show a) => a -> ASM6502 ()
jmpp = op16 0x6c

jsr :: (Integral a, Show a) => a -> ASM6502 ()
jsr = op16 0x20

ldai :: (Integral a, Show a) => a -> ASM6502 ()
ldai = op8 0xa9
ldaz :: (Integral a, Show a) => a -> ASM6502 ()
ldaz = op8 0xa5
ldam :: (Integral a, Show a) => a -> ASM6502 ()
ldam = op16 0xad
lda :: (Integral a, Show a, Bounded a) => a -> ASM6502 ()
lda = op8or16 0xa5 0xad
ldaxz :: (Integral a, Show a) => a -> ASM6502 ()
ldaxz = op8 0xb5
ldaxm :: (Integral a, Show a) => a -> ASM6502 ()
ldaxm = op16 0xbd
ldax :: (Integral a, Show a, Bounded a) => a -> ASM6502 ()
ldax = op8or16 0xb5 0xbd
lday :: (Integral a, Show a) => a -> ASM6502 ()
lday = op16 0xb9
ldapx :: (Integral a, Show a) => a -> ASM6502 ()
ldapx = op8 0xa1
ldayp :: (Integral a, Show a) => a -> ASM6502 ()
ldayp = op8 0xb1

ldxi :: (Integral a, Show a) => a -> ASM6502 ()
ldxi = op8 0xa2
ldxz :: (Integral a, Show a) => a -> ASM6502 ()
ldxz = op8 0xa6
ldxm :: (Integral a, Show a) => a -> ASM6502 ()
ldxm = op16 0xae
ldx :: (Integral a, Show a, Bounded a) => a -> ASM6502 ()
ldx = op8or16 0xa6 0xae
ldxyz :: (Integral a, Show a) => a -> ASM6502 ()
ldxyz = op8 0xb6
ldxym :: (Integral a, Show a) => a -> ASM6502 ()
ldxym = op16 0xbe
ldxy :: (Integral a, Show a, Bounded a) => a -> ASM6502 ()
ldxy = op8or16 0xb6 0xbe

ldyi :: (Integral a, Show a) => a -> ASM6502 ()
ldyi = op8 0xa0
ldyz :: (Integral a, Show a) => a -> ASM6502 ()
ldyz = op8 0xa4
ldym :: (Integral a, Show a) => a -> ASM6502 ()
ldym = op16 0xac
ldy :: (Integral a, Show a, Bounded a) => a -> ASM6502 ()
ldy = op8or16 0xa4 0xac
ldyxz :: (Integral a, Show a) => a -> ASM6502 ()
ldyxz = op8 0xb4
ldyxm :: (Integral a, Show a) => a -> ASM6502 ()
ldyxm = op16 0xbc
ldyx :: (Integral a, Show a, Bounded a) => a -> ASM6502 ()
ldyx = op8or16 0xb4 0xbc

lsra :: ASM6502 ()
lsra = byte 0x4a
lsrz :: (Integral a, Show a) => a -> ASM6502 ()
lsrz = op8 0x46
lsrm :: (Integral a, Show a) => a -> ASM6502 ()
lsrm = op16 0x4e
lsr :: (Integral a, Show a, Bounded a) => a -> ASM6502 ()
lsr = op8or16 0x46 0x4e
lsrxz :: (Integral a, Show a) => a -> ASM6502 ()
lsrxz = op8 0x56
lsrxm :: (Integral a, Show a) => a -> ASM6502 ()
lsrxm = op16 0x5e
lsrx :: (Integral a, Show a, Bounded a) => a -> ASM6502 ()
lsrx = op8or16 0x56 0x5e

nop :: ASM6502 ()
nop = byte 0xea

orai :: (Integral a, Show a) => a -> ASM6502 ()
orai = op8 0x09
oraz :: (Integral a, Show a) => a -> ASM6502 ()
oraz = op8 0x05
oram :: (Integral a, Show a) => a -> ASM6502 ()
oram = op16 0x0d
ora :: (Integral a, Show a, Bounded a) => a -> ASM6502 ()
ora = op8or16 0x05 0x0d
oraxz :: (Integral a, Show a) => a -> ASM6502 ()
oraxz = op8 0x15
oraxm :: (Integral a, Show a) => a -> ASM6502 ()
oraxm = op16 0x1d
orax :: (Integral a, Show a, Bounded a) => a -> ASM6502 ()
orax = op8or16 0x15 0x1d
oray :: (Integral a, Show a) => a -> ASM6502 ()
oray = op16 0x19
orapx :: (Integral a, Show a) => a -> ASM6502 ()
orapx = op8 0x01
orayp :: (Integral a, Show a) => a -> ASM6502 ()
orayp = op8 0x11

pha :: ASM6502 ()
pha = byte 0x48

php :: ASM6502 ()
php = byte 0x08

pla :: ASM6502 ()
pla = byte 0x68

plp :: ASM6502 ()
plp = byte 0x28

rola :: ASM6502 ()
rola = byte 0x2a
rolz :: (Integral a, Show a) => a -> ASM6502 ()
rolz = op8 0x26
rolm :: (Integral a, Show a) => a -> ASM6502 ()
rolm = op16 0x2e
rol :: (Integral a, Show a, Bounded a) => a -> ASM6502 ()
rol = op8or16 0x26 0x2e
rolxz :: (Integral a, Show a) => a -> ASM6502 ()
rolxz = op8 0x36
rolxm :: (Integral a, Show a) => a -> ASM6502 ()
rolxm = op16 0x3e
rolx :: (Integral a, Show a, Bounded a) => a -> ASM6502 ()
rolx = op8or16 0x36 0x3e

rora :: ASM6502 ()
rora = byte 0x6a
rorz :: (Integral a, Show a) => a -> ASM6502 ()
rorz = op8 0x66
rorm :: (Integral a, Show a) => a -> ASM6502 ()
rorm = op16 0x6e
ror :: (Integral a, Show a, Bounded a) => a -> ASM6502 ()
ror = op8or16 0x66 0x6e
rorxz :: (Integral a, Show a) => a -> ASM6502 ()
rorxz = op8 0x76
rorxm :: (Integral a, Show a) => a -> ASM6502 ()
rorxm = op16 0x7e
rorx :: (Integral a, Show a, Bounded a) => a -> ASM6502 ()
rorx = op8or16 0x76 0x7e

rti :: ASM6502 ()
rti = byte 0x40

rts :: ASM6502 ()
rts = byte 0x60

sbci :: (Integral a, Show a) => a -> ASM6502 ()
sbci = op8 0xe9
sbcz :: (Integral a, Show a) => a -> ASM6502 ()
sbcz = op8 0xe5
sbcm :: (Integral a, Show a) => a -> ASM6502 ()
sbcm = op16 0xed
sbc :: (Integral a, Show a, Bounded a) => a -> ASM6502 ()
sbc = op8or16 0xe5 0xed
sbcxz :: (Integral a, Show a) => a -> ASM6502 ()
sbcxz = op8 0xf5
sbcxm :: (Integral a, Show a) => a -> ASM6502 ()
sbcxm = op16 0xfd
sbcx :: (Integral a, Show a, Bounded a) => a -> ASM6502 ()
sbcx = op8or16 0xf5 0xfd
sbcy :: (Integral a, Show a) => a -> ASM6502 ()
sbcy = op16 0xf9
sbcpx :: (Integral a, Show a) => a -> ASM6502 ()
sbcpx = op8 0xe1
sbcyp :: (Integral a, Show a) => a -> ASM6502 ()
sbcyp = op8 0xf1

sec :: ASM6502 ()
sec = byte 0x38

sed :: ASM6502 ()
sed = byte 0xf8

sei :: ASM6502 ()
sei = byte 0x78

staz :: (Integral a, Show a) => a -> ASM6502 ()
staz = op8 0x85
stam :: (Integral a, Show a) => a -> ASM6502 ()
stam = op16 0x8d
sta :: (Integral a, Show a, Bounded a) => a -> ASM6502 ()
sta = op8or16 0x85 0x8d
staxz :: (Integral a, Show a) => a -> ASM6502 ()
staxz = op8 0x95
staxm :: (Integral a, Show a) => a -> ASM6502 ()
staxm = op16 0x9d
stax :: (Integral a, Show a, Bounded a) => a -> ASM6502 ()
stax = op8or16 0x95 0x9d
stay :: (Integral a, Show a) => a -> ASM6502 ()
stay = op16 0x99
stapx :: (Integral a, Show a) => a -> ASM6502 ()
stapx = op8 0x81
stayp :: (Integral a, Show a) => a -> ASM6502 ()
stayp = op8 0x91

stxz :: (Integral a, Show a) => a -> ASM6502 ()
stxz = op8 0x86
stxm :: (Integral a, Show a) => a -> ASM6502 ()
stxm = op16 0x8e
stx :: (Integral a, Show a, Bounded a) => a -> ASM6502 ()
stx = op8or16 0x86 0x8e
stxy :: (Integral a, Show a) => a -> ASM6502 ()
stxy = op8 0x8e

styz :: (Integral a, Show a) => a -> ASM6502 ()
styz = op8 0x84
stym :: (Integral a, Show a) => a -> ASM6502 ()
stym = op16 0x8c
sty :: (Integral a, Show a, Bounded a) => a -> ASM6502 ()
sty = op8or16 0x84 0x8c
styx :: (Integral a, Show a) => a -> ASM6502 ()
styx = op8 0x94

tax :: ASM6502 ()
tax = byte 0xaa

tay :: ASM6502 ()
tay = byte 0xa8

tsx :: ASM6502 ()
tsx = byte 0xba

txa :: ASM6502 ()
txa = byte 0x8a

txs :: ASM6502 ()
txs = byte 0x9a

tya :: ASM6502 ()
tya = byte 0x98

 -- Nice shortcuts

addi x = clc >> adci x
addz x = clc >> adcz x
addm x = clc >> adcm x
add x = clc >> adc x

subi x = sec >> sbci x
subz x = sec >> sbcz x
subm x = sec >> sbcm x
sub x = sec >> sbc x

forinxin res = repfor (ldxi 0x00) (inx >> cpxi (size res) >>. bne)
forinyin res = repfor (ldyi 0x00) (iny >> cpyi (size res) >>. bne)
 -- NOTE: These can only work for resources of size <= 0x80
fordexin res = repfor (ldxi (size res - 1)) (dex >>. bpl)
fordeyin res = repfor (ldyi (size res - 1)) (dey >>. bpl)

infix 2 ->*
infix 2 -&>*
infix 2 -|>*
infix 2 -^>*
infix 2 -+>*
infix 2 -->*
infix 2 *<-
infix 2 *<&-
infix 2 *<|-
infix 2 *<^-
infix 2 *<+-
infix 2 *<--
infix 2 *->*
infix 2 *-&>*
infix 2 *-|>*
infix 2 *-^>*
infix 2 *-+>*
infix 2 *-->*
infix 2 *<-*
infix 2 *<&-*
infix 2 *<|-*
infix 2 *<^-*
infix 2 *<+-*
infix 2 *<--*

(->*) :: (Integral a, Show a, Bounded a) => Word8 -> a -> ASM6502 ()
val ->* mem = ldai val >> sta mem
(-&>*) :: (Integral a, Show a, Bounded a) => Word8 -> a -> ASM6502 ()
val -&>* mem = lda mem >> andi val >> sta mem
(-|>*) :: (Integral a, Show a, Bounded a) => Word8 -> a -> ASM6502 ()
val -|>* mem = lda mem >> orai val >> sta mem
(-^>*) :: (Integral a, Show a, Bounded a) => Word8 -> a -> ASM6502 ()
val -^>* mem = lda mem >> eori val >> sta mem
(-+>*) :: (Integral a, Show a, Bounded a) => Word8 -> a -> ASM6502 ()
val -+>* mem = lda mem >> addi val >> sta mem
(-->*) :: (Integral a, Show a, Bounded a) => Word8 -> a -> ASM6502 ()
val -->* mem = lda mem >> subi val >> sta mem

(*<-) :: (Integral a, Show a, Bounded a) => a -> Word8 -> ASM6502 ()
(*<-) = flip (->*)
(*<&-) :: (Integral a, Show a, Bounded a) => a -> Word8 -> ASM6502 ()
(*<&-) = flip (-&>*)
(*<|-) :: (Integral a, Show a, Bounded a) => a -> Word8 -> ASM6502 ()
(*<|-) = flip (-|>*)
(*<^-) :: (Integral a, Show a, Bounded a) => a -> Word8 -> ASM6502 ()
(*<^-) = flip (-^>*)
(*<+-) :: (Integral a, Show a, Bounded a) => a -> Word8 -> ASM6502 ()
(*<+-) = flip (-+>*)
(*<--) :: (Integral a, Show a, Bounded a) => a -> Word8 -> ASM6502 ()
(*<--) = flip (-->*)

(*->*) :: (Integral a, Show a, Bounded a, Integral b, Show b, Bounded b) => a -> b -> ASM6502 ()
from *->* to = lda from >> sta to
(*-&>*) :: (Integral a, Show a, Bounded a, Integral b, Show b, Bounded b) => a -> b -> ASM6502 ()
from *-&>* to = lda to >> and from >> sta to
(*-|>*) :: (Integral a, Show a, Bounded a, Integral b, Show b, Bounded b) => a -> b -> ASM6502 ()
from *-|>* to = lda to >> ora from >> sta to
(*-^>*) :: (Integral a, Show a, Bounded a, Integral b, Show b, Bounded b) => a -> b -> ASM6502 ()
from *-^>* to = lda to >> eor from >> sta to
(*-+>*) :: (Integral a, Show a, Bounded a, Integral b, Show b, Bounded b) => a -> b -> ASM6502 ()
from *-+>* to = lda to >> add from >> sta to
(*-->*) :: (Integral a, Show a, Bounded a, Integral b, Show b, Bounded b) => a -> b -> ASM6502 ()
from *-->* to = lda to >> sub from >> sta to

(*<-*) :: (Integral a, Show a, Bounded a, Integral b, Show b, Bounded b) => b -> a -> ASM6502 ()
(*<-*) = flip (*->*)
(*<&-*) :: (Integral a, Show a, Bounded a, Integral b, Show b, Bounded b) => b -> a -> ASM6502 ()
(*<&-*) = flip (*-&>*)
(*<|-*) :: (Integral a, Show a, Bounded a, Integral b, Show b, Bounded b) => b -> a -> ASM6502 ()
(*<|-*) = flip (*-|>*)
(*<^-*) :: (Integral a, Show a, Bounded a, Integral b, Show b, Bounded b) => b -> a -> ASM6502 ()
(*<^-*) = flip (*-^>*)
(*<+-*) :: (Integral a, Show a, Bounded a, Integral b, Show b, Bounded b) => b -> a -> ASM6502 ()
(*<+-*) = flip (*-+>*)
(*<--*) :: (Integral a, Show a, Bounded a, Integral b, Show b, Bounded b) => b -> a -> ASM6502 ()
(*<--*) = flip (*-->*)

type Res6502 = Res Word16
res6502 :: Word16 -> [Word16] -> [Res6502]
res6502 = resources

