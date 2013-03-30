
module ASM6502 where

import Prelude hiding (and)
import qualified Prelude (and)
import Data.Word
import Data.Int
import qualified Data.Bits as B
import qualified Data.Sequence as S
import Assembly
import ASM
import Text.Printf

type ASM6502 a = ASM Word16 a
type Section6502 a = ASMSection Word16 a
type ASM6502Section a = ASMSection Word16 a

low :: Integral a => a -> Word8
low = fromIntegral
high :: Integral a => a -> Word8
high x = fromIntegral (B.shiftR (fromIntegral x :: Word16) 8)

op8 :: Integral x => String -> Word8 -> x -> ASM6502 ()
op8 name a x = byte a >> Assembler f where
    f pos = let
        res = case no_overflow x :: Maybe Word8 of
            Just w8 -> S.singleton w8
            Nothing -> error$ printf "Overflow error in argument to %s at 0x%x (0x%x)" name (toInteger pos) (toInteger x)
        in (pos + 1, res, ())

op16 :: Integral x => String -> Word8 -> x -> ASM6502 ()
op16 name a x = byte a >> Assembler f where
    f pos = let
        res16 = case no_overflow x :: Maybe Word8 of
            Just w8 -> fromIntegral w8
            Nothing -> case no_overflow x :: Maybe Word16 of
                Just w16 -> w16
                Nothing -> error$ printf "Overflow error in argument to %s at 0x%x (0x%x)" name (toInteger pos) (toInteger x)
        res = S.singleton (fromIntegral res16) S.>< S.singleton (fromIntegral (B.shiftR res16 8))
        in (pos + 2, res, ())

 -- case (maxBound of argument type) of
 --    8bit -> generate op8
 --    16bit -> generate op16
 --    other -> generate op8 or op16 depending on argument's value
 -- This last case will lead to an infinite loop if the argument
 --  depends on the size of the generated op.  Fortunately, arguments with
 --  that dependency are likely to be ASM labels, which are 16bit words.
op8or16 :: (Integral x, Bounded x) => String -> Word8 -> Word8 -> x -> ASM6502 ()
op8or16 = op8or16' maxBound

op8or16' :: (Integral x, Bounded x) => x -> String -> Word8 -> Word8 -> x -> ASM6502 ()
op8or16' max name a b x = case toInteger max of
    255 -> byte a >> byte (fromIntegral x)
    127 -> byte a >> byte (fromIntegral x)
    65535 -> byte b >> le16 (fromIntegral x)
    32767 -> byte b >> le16 (fromIntegral x)
    _ -> case no_overflow x :: Maybe Word8 of
        Just w8 -> byte a >> byte w8
        Nothing -> case no_overflow x :: Maybe Word16 of
            Just w16 -> byte b >> le16 w16
            Nothing -> Assembler f where
                f pos = error$ printf "Overflow error in argument to %s at 0x%x (0x%x)" name (toInteger pos) (toInteger x)

 -- HACK HACK HACK
instance Bounded Integer where
    maxBound = -1
    minBound = error$ "I'm sorry!  I had to define Bounded Integer to make op8or16 work!  Wait, why are you trying to get minBound :: Integer anyway?"


rel8 :: Integral a => String -> Word8 -> a -> ASM6502 ()
rel8 name b x = byte b >> Assembler f where
    f pos = let
        off = fromIntegral x - fromIntegral (pos + 1)
        res = case no_overflow off :: Maybe Int8 of
            Just i8 -> S.singleton (fromIntegral i8)
            Nothing -> fail$ printf "Branch target is too far away in %s at 0x%x (0x%x)" name (toInteger pos) (toInteger off)
        in (pos + 1, res, ())

adci :: Integral a => a -> ASM6502 ()
adci = op8 "adci" 0x69
adcz :: Integral a => a -> ASM6502 ()
adcz = op8 "adcz" 0x65
adcm :: Integral a => a -> ASM6502 ()
adcm = op16 "adcm" 0x6d
adc :: (Integral a, Bounded a) => a -> ASM6502 ()
adc = op8or16 "adc" 0x65 0x6d
adcxz :: Integral a => a -> ASM6502 ()
adcxz = op8 "adcxz" 0x75
adcxm :: Integral a => a -> ASM6502 ()
adcxm = op16 "adcxm" 0x7d
adcx :: (Integral a, Bounded a) => a -> ASM6502 ()
adcx = op8or16 "adcx" 0x75 0x7d
adcy :: Integral a => a -> ASM6502 ()
adcy = op16 "adcy" 0x79
adcxp :: Integral a => a -> ASM6502 ()
adcxp = op8 "adcxp" 0x61
adcpy :: Integral a => a -> ASM6502 ()
adcpy = op8 "adcpy" 0x71

andi :: Integral a => a -> ASM6502 ()
andi = op8 "andi" 0x29
andz :: Integral a => a -> ASM6502 ()
andz = op8 "andz" 0x25
andm :: Integral a => a -> ASM6502 ()
andm = op16 "andm" 0x2d
and :: (Integral a, Bounded a) => a -> ASM6502 ()
and = op8or16 "and" 0x25 0x2d
andxz :: Integral a => a -> ASM6502 ()
andxz = op8 "andxz" 0x35
andxm :: Integral a => a -> ASM6502 ()
andxm = op16 "andxm" 0x3d
andx :: (Integral a, Bounded a) => a -> ASM6502 ()
andx = op8or16 "andx" 0x35 0x3d
andy :: Integral a => a -> ASM6502 ()
andy = op16 "andy" 0x39
andpx :: Integral a => a -> ASM6502 ()
andpx = op8 "andpx" 0x21
andyp :: Integral a => a -> ASM6502 ()
andyp = op8 "andyp" 0x31

asla :: ASM6502 ()
asla = byte 0x0a
aslz :: Integral a => a -> ASM6502 ()
aslz = op8 "aslz" 0x06
aslm :: Integral a => a -> ASM6502 ()
aslm = op16 "aslm" 0x0e
asl :: (Integral a, Bounded a) => a -> ASM6502 ()
asl = op8or16 "asl" 0x06 0x0e
aslxz :: Integral a => a -> ASM6502 ()
aslxz = op8 "aslxz" 0x16
aslxm :: Integral a => a -> ASM6502 ()
aslxm = op16 "aslxm" 0x1e
aslx :: (Integral a, Bounded a) => a -> ASM6502 ()
aslx = op8or16 "aslx" 0x16 0x1e

bcc :: Integral a => a -> ASM6502 ()
bcc = rel8 "bcc" 0x90

bcs :: Integral a => a -> ASM6502 ()
bcs = rel8 "bcs" 0xb0

beq :: Integral a => a -> ASM6502 ()
beq = rel8 "beq" 0xf0

bitz :: Integral a => a -> ASM6502 ()
bitz = op8 "bitz" 0x24
bitm :: Integral a => a -> ASM6502 ()
bitm = op16 "bitm" 0x2c
bit :: (Integral a, Bounded a) => a -> ASM6502 ()
bit = op8or16 "bit" 0x24 0x2c

bmi :: Integral a => a -> ASM6502 ()
bmi = rel8 "bmi" 0x30

bne :: Integral a => a -> ASM6502 ()
bne = rel8 "bne" 0xd0

bpl :: Integral a => a -> ASM6502 ()
bpl = rel8 "bpl" 0x10

brk :: ASM6502 ()
brk = byte 0x00

bvc :: Integral a => a -> ASM6502 ()
bvc = rel8 "bvc" 0x50

bvs :: Integral a => a -> ASM6502 ()
bvs = rel8 "bvs" 0x70

clc :: ASM6502 ()
clc = byte 0x18

cld :: ASM6502 ()
cld = byte 0xd8

cli :: ASM6502 ()
cli = byte 0x58

clv :: ASM6502 ()
clv = byte 0xb8

cmpi :: Integral a => a -> ASM6502 ()
cmpi = op8 "cmpi" 0xc9

cmpz :: Integral a => a -> ASM6502 ()
cmpz = op8 "cmpz" 0xc5
cmpm :: Integral a => a -> ASM6502 ()
cmpm = op16 "cmpm" 0xcd
cmp :: (Integral a, Bounded a) => a -> ASM6502 ()
cmp = op8or16 "cmp" 0xc5 0xcd
cmpxz :: Integral a => a -> ASM6502 ()
cmpxz = op8 "cmpxz" 0xd5
cmpxm :: Integral a => a -> ASM6502 ()
cmpxm = op16 "cmpxm" 0xdd
cmpx :: (Integral a, Bounded a) => a -> ASM6502 ()
cmpx = op8or16 "cmpx" 0xd5 0xdd
cmpy :: Integral a => a -> ASM6502 ()
cmpy = op16 "cmpy" 0xd9
cmppx :: Integral a => a -> ASM6502 ()
cmppx = op8 "cmppx" 0xc1
cmpyp :: Integral a => a -> ASM6502 ()
cmpyp = op8 "cmpyp" 0xd1

cpxi :: Integral a => a -> ASM6502 ()
cpxi = op8 "cpxi" 0xe0
cpxz :: Integral a => a -> ASM6502 ()
cpxz = op8 "cpxz" 0xe4
cpxm :: Integral a => a -> ASM6502 ()
cpxm = op16 "cpxm" 0xec
cpx :: (Integral a, Bounded a) => a -> ASM6502 ()
cpx = op8or16 "cpx" 0xe4 0xec

cpyi :: Integral a => a -> ASM6502 ()
cpyi = op8 "cpyi" 0xc0
cpyz :: Integral a => a -> ASM6502 ()
cpyz = op8 "cpyz" 0xc4
cpym :: Integral a => a -> ASM6502 ()
cpym = op16 "cpym" 0xcc
cpy :: (Integral a, Bounded a) => a -> ASM6502 ()
cpy = op8or16 "cpy" 0xc4 0xcc

decz :: Integral a => a -> ASM6502 ()
decz = op8 "decz" 0xc6
decm :: Integral a => a -> ASM6502 ()
decm = op16 "decm" 0xce
dec :: (Integral a, Bounded a) => a -> ASM6502 ()
dec = op8or16 "dec" 0xc6 0xce
decxz :: Integral a => a -> ASM6502 ()
decxz = op8 "decxz" 0xd6
decxm :: Integral a => a -> ASM6502 ()
decxm = op16 "decxm" 0xde
decx :: (Integral a, Bounded a) => a -> ASM6502 ()
decx = op8or16 "decx" 0xd6 0xde

dex :: ASM6502 ()
dex = byte 0xca

dey :: ASM6502 ()
dey = byte 0x88

eori :: Integral a => a -> ASM6502 ()
eori = op8 "eori" 0x49
eorz :: Integral a => a -> ASM6502 ()
eorz = op8 "eorz" 0x45
eorm :: Integral a => a -> ASM6502 ()
eorm = op16 "eorm" 0x4d
eor :: (Integral a, Bounded a) => a -> ASM6502 ()
eor = op8or16 "eor" 0x45 0x4d
eorxz :: Integral a => a -> ASM6502 ()
eorxz = op8 "eorxz" 0x55
eorxm :: Integral a => a -> ASM6502 ()
eorxm = op16 "eorxm" 0x5d
eorx :: (Integral a, Bounded a) => a -> ASM6502 ()
eorx = op8or16 "eorx" 0x55 0x5d
eory :: Integral a => a -> ASM6502 ()
eory = op16 "eory" 0x59
eorpx :: Integral a => a -> ASM6502 ()
eorpx = op8 "eorpx" 0x41
eoryp :: Integral a => a -> ASM6502 ()
eoryp = op8 "eoryp" 0x51

incz :: Integral a => a -> ASM6502 ()
incz = op8 "incz" 0xe6
incm :: Integral a => a -> ASM6502 ()
incm = op16 "incm" 0xee
inc :: (Integral a, Bounded a) => a -> ASM6502 ()
inc = op8or16 "inc" 0xe6 0xee
incxz :: Integral a => a -> ASM6502 ()
incxz = op8 "incxz" 0xf6
incxm :: Integral a => a -> ASM6502 ()
incxm = op16 "incxm" 0xfe
incx :: (Integral a, Bounded a) => a -> ASM6502 ()
incx = op8or16 "incx" 0xf6 0xfe

inx :: ASM6502 ()
inx = byte 0xe8

iny :: ASM6502 ()
iny = byte 0xc8

jmp :: Integral a => a -> ASM6502 ()
jmp = op16 "jmp" 0x4c

jmpp :: Integral a => a -> ASM6502 ()
jmpp = op16 "jmpp" 0x6c

jsr :: Integral a => a -> ASM6502 ()
jsr = op16 "jsr" 0x20

ldai :: Integral a => a -> ASM6502 ()
ldai = op8 "ldai" 0xa9
ldaz :: Integral a => a -> ASM6502 ()
ldaz = op8 "ldaz" 0xa5
ldam :: Integral a => a -> ASM6502 ()
ldam = op16 "ldam" 0xad
lda :: (Integral a, Bounded a) => a -> ASM6502 ()
lda = op8or16 "lda" 0xa5 0xad
ldaxz :: Integral a => a -> ASM6502 ()
ldaxz = op8 "ldaxz" 0xb5
ldaxm :: Integral a => a -> ASM6502 ()
ldaxm = op16 "ldaxm" 0xbd
ldax :: (Integral a, Bounded a) => a -> ASM6502 ()
ldax = op8or16 "ldax" 0xb5 0xbd
lday :: Integral a => a -> ASM6502 ()
lday = op16 "lday" 0xb9
ldapx :: Integral a => a -> ASM6502 ()
ldapx = op8 "ldapx" 0xa1
ldayp :: Integral a => a -> ASM6502 ()
ldayp = op8 "ldayp" 0xb1

ldxi :: Integral a => a -> ASM6502 ()
ldxi = op8 "ldxi" 0xa2
ldxz :: Integral a => a -> ASM6502 ()
ldxz = op8 "ldxz" 0xa6
ldxm :: Integral a => a -> ASM6502 ()
ldxm = op16 "ldxm" 0xae
ldx :: (Integral a, Bounded a) => a -> ASM6502 ()
ldx = op8or16 "ldx" 0xa6 0xae
ldxyz :: Integral a => a -> ASM6502 ()
ldxyz = op8 "ldxyz" 0xb6
ldxym :: Integral a => a -> ASM6502 ()
ldxym = op16 "ldxym" 0xbe
ldxy :: (Integral a, Bounded a) => a -> ASM6502 ()
ldxy = op8or16 "ldxy" 0xb6 0xbe

ldyi :: Integral a => a -> ASM6502 ()
ldyi = op8 "ldyi" 0xa0
ldyz :: Integral a => a -> ASM6502 ()
ldyz = op8 "ldyz" 0xa4
ldym :: Integral a => a -> ASM6502 ()
ldym = op16 "ldym" 0xac
ldy :: (Integral a, Bounded a) => a -> ASM6502 ()
ldy = op8or16 "ldy" 0xa4 0xac
ldyxz :: Integral a => a -> ASM6502 ()
ldyxz = op8 "ldyxz" 0xb4
ldyxm :: Integral a => a -> ASM6502 ()
ldyxm = op16 "ldyxm" 0xbc
ldyx :: (Integral a, Bounded a) => a -> ASM6502 ()
ldyx = op8or16 "ldyx" 0xb4 0xbc

lsra :: ASM6502 ()
lsra = byte 0x4a
lsrz :: Integral a => a -> ASM6502 ()
lsrz = op8 "lsrz" 0x46
lsrm :: Integral a => a -> ASM6502 ()
lsrm = op16 "lsrm" 0x4e
lsr :: (Integral a, Bounded a) => a -> ASM6502 ()
lsr = op8or16 "lsr" 0x46 0x4e
lsrxz :: Integral a => a -> ASM6502 ()
lsrxz = op8 "lsrxz" 0x56
lsrxm :: Integral a => a -> ASM6502 ()
lsrxm = op16 "lsrxm" 0x5e
lsrx :: (Integral a, Bounded a) => a -> ASM6502 ()
lsrx = op8or16 "lsrx" 0x56 0x5e

nop :: ASM6502 ()
nop = byte 0xea

orai :: Integral a => a -> ASM6502 ()
orai = op8 "orai" 0x09
oraz :: Integral a => a -> ASM6502 ()
oraz = op8 "oraz" 0x05
oram :: Integral a => a -> ASM6502 ()
oram = op16 "oram" 0x0d
ora :: (Integral a, Bounded a) => a -> ASM6502 ()
ora = op8or16 "ora" 0x05 0x0d
oraxz :: Integral a => a -> ASM6502 ()
oraxz = op8 "oraxz" 0x15
oraxm :: Integral a => a -> ASM6502 ()
oraxm = op16 "oraxm" 0x1d
orax :: (Integral a, Bounded a) => a -> ASM6502 ()
orax = op8or16 "orax" 0x15 0x1d
oray :: Integral a => a -> ASM6502 ()
oray = op16 "oray" 0x19
orapx :: Integral a => a -> ASM6502 ()
orapx = op8 "orapx" 0x01
orayp :: Integral a => a -> ASM6502 ()
orayp = op8 "orayp" 0x11

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
rolz :: Integral a => a -> ASM6502 ()
rolz = op8 "rolz" 0x26
rolm :: Integral a => a -> ASM6502 ()
rolm = op16 "rolm" 0x2e
rol :: (Integral a, Bounded a) => a -> ASM6502 ()
rol = op8or16 "rol" 0x26 0x2e
rolxz :: Integral a => a -> ASM6502 ()
rolxz = op8 "rolxz" 0x36
rolxm :: Integral a => a -> ASM6502 ()
rolxm = op16 "rolxm" 0x3e
rolx :: (Integral a, Bounded a) => a -> ASM6502 ()
rolx = op8or16 "rolx" 0x36 0x3e

rora :: ASM6502 ()
rora = byte 0x6a
rorz :: Integral a => a -> ASM6502 ()
rorz = op8 "rorz" 0x66
rorm :: Integral a => a -> ASM6502 ()
rorm = op16 "rorm" 0x6e
ror :: (Integral a, Bounded a) => a -> ASM6502 ()
ror = op8or16 "ror" 0x66 0x6e
rorxz :: Integral a => a -> ASM6502 ()
rorxz = op8 "rorxz" 0x76
rorxm :: Integral a => a -> ASM6502 ()
rorxm = op16 "rorxm" 0x7e
rorx :: (Integral a, Bounded a) => a -> ASM6502 ()
rorx = op8or16 "rorx" 0x76 0x7e

rti :: ASM6502 ()
rti = byte 0x40

rts :: ASM6502 ()
rts = byte 0x60

sbci :: Integral a => a -> ASM6502 ()
sbci = op8 "sbci" 0xe9
sbcz :: Integral a => a -> ASM6502 ()
sbcz = op8 "sbcz" 0xe5
sbcm :: Integral a => a -> ASM6502 ()
sbcm = op16 "sbcm" 0xed
sbc :: (Integral a, Bounded a) => a -> ASM6502 ()
sbc = op8or16 "sbc" 0xe5 0xed
sbcxz :: Integral a => a -> ASM6502 ()
sbcxz = op8 "sbcxz" 0xf5
sbcxm :: Integral a => a -> ASM6502 ()
sbcxm = op16 "sbcxm" 0xfd
sbcx :: (Integral a, Bounded a) => a -> ASM6502 ()
sbcx = op8or16 "sbcx" 0xf5 0xfd
sbcy :: Integral a => a -> ASM6502 ()
sbcy = op16 "sbcy" 0xf9
sbcpx :: Integral a => a -> ASM6502 ()
sbcpx = op8 "sbcpx" 0xe1
sbcyp :: Integral a => a -> ASM6502 ()
sbcyp = op8 "sbcyp" 0xf1

sec :: ASM6502 ()
sec = byte 0x38

sed :: ASM6502 ()
sed = byte 0xf8

sei :: ASM6502 ()
sei = byte 0x78

staz :: Integral a => a -> ASM6502 ()
staz = op8 "staz" 0x85
stam :: Integral a => a -> ASM6502 ()
stam = op16 "stam" 0x8d
sta :: (Integral a, Bounded a) => a -> ASM6502 ()
sta = op8or16 "sta" 0x85 0x8d
staxz :: Integral a => a -> ASM6502 ()
staxz = op8 "staxz" 0x95
staxm :: Integral a => a -> ASM6502 ()
staxm = op16 "staxm" 0x9d
stax :: (Integral a, Bounded a) => a -> ASM6502 ()
stax = op8or16 "stax" 0x95 0x9d
stay :: Integral a => a -> ASM6502 ()
stay = op16 "stay" 0x99
stapx :: Integral a => a -> ASM6502 ()
stapx = op8 "stapx" 0x81
stayp :: Integral a => a -> ASM6502 ()
stayp = op8 "stayp" 0x91

stxz :: Integral a => a -> ASM6502 ()
stxz = op8 "stxz" 0x86
stxm :: Integral a => a -> ASM6502 ()
stxm = op16 "stxm" 0x8e
stx :: (Integral a, Bounded a) => a -> ASM6502 ()
stx = op8or16 "stx" 0x86 0x8e
stxy :: Integral a => a -> ASM6502 ()
stxy = op8 "stxy" 0x8e

styz :: Integral a => a -> ASM6502 ()
styz = op8 "styz" 0x84
stym :: Integral a => a -> ASM6502 ()
stym = op16 "stym" 0x8c
sty :: (Integral a, Bounded a) => a -> ASM6502 ()
sty = op8or16 "sty" 0x84 0x8c
styx :: Integral a => a -> ASM6502 ()
styx = op8 "styx" 0x94

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

(->*) :: (Integral a, Bounded a) => Word8 -> a -> ASM6502 ()
val ->* mem = ldai val >> sta mem
(-&>*) :: (Integral a, Bounded a) => Word8 -> a -> ASM6502 ()
val -&>* mem = lda mem >> andi val >> sta mem
(-|>*) :: (Integral a, Bounded a) => Word8 -> a -> ASM6502 ()
val -|>* mem = lda mem >> orai val >> sta mem
(-^>*) :: (Integral a, Bounded a) => Word8 -> a -> ASM6502 ()
val -^>* mem = lda mem >> eori val >> sta mem
(-+>*) :: (Integral a, Bounded a) => Word8 -> a -> ASM6502 ()
val -+>* mem = lda mem >> addi val >> sta mem
(-->*) :: (Integral a, Bounded a) => Word8 -> a -> ASM6502 ()
val -->* mem = lda mem >> subi val >> sta mem

(*<-) :: (Integral a, Bounded a) => a -> Word8 -> ASM6502 ()
(*<-) = flip (->*)
(*<&-) :: (Integral a, Bounded a) => a -> Word8 -> ASM6502 ()
(*<&-) = flip (-&>*)
(*<|-) :: (Integral a, Bounded a) => a -> Word8 -> ASM6502 ()
(*<|-) = flip (-|>*)
(*<^-) :: (Integral a, Bounded a) => a -> Word8 -> ASM6502 ()
(*<^-) = flip (-^>*)
(*<+-) :: (Integral a, Bounded a) => a -> Word8 -> ASM6502 ()
(*<+-) = flip (-+>*)
(*<--) :: (Integral a, Bounded a) => a -> Word8 -> ASM6502 ()
(*<--) = flip (-->*)

(*->*) :: (Integral a, Bounded a, Integral b, Bounded b) => a -> b -> ASM6502 ()
from *->* to = lda from >> sta to
(*-&>*) :: (Integral a, Bounded a, Integral b, Bounded b) => a -> b -> ASM6502 ()
from *-&>* to = lda to >> and from >> sta to
(*-|>*) :: (Integral a, Bounded a, Integral b, Bounded b) => a -> b -> ASM6502 ()
from *-|>* to = lda to >> ora from >> sta to
(*-^>*) :: (Integral a, Bounded a, Integral b, Bounded b) => a -> b -> ASM6502 ()
from *-^>* to = lda to >> eor from >> sta to
(*-+>*) :: (Integral a, Bounded a, Integral b, Bounded b) => a -> b -> ASM6502 ()
from *-+>* to = lda to >> add from >> sta to
(*-->*) :: (Integral a, Bounded a, Integral b, Bounded b) => a -> b -> ASM6502 ()
from *-->* to = lda to >> sub from >> sta to

(*<-*) :: (Integral a, Bounded a, Integral b, Bounded b) => b -> a -> ASM6502 ()
(*<-*) = flip (*->*)
(*<&-*) :: (Integral a, Bounded a, Integral b, Bounded b) => b -> a -> ASM6502 ()
(*<&-*) = flip (*-&>*)
(*<|-*) :: (Integral a, Bounded a, Integral b, Bounded b) => b -> a -> ASM6502 ()
(*<|-*) = flip (*-|>*)
(*<^-*) :: (Integral a, Bounded a, Integral b, Bounded b) => b -> a -> ASM6502 ()
(*<^-*) = flip (*-^>*)
(*<+-*) :: (Integral a, Bounded a, Integral b, Bounded b) => b -> a -> ASM6502 ()
(*<+-*) = flip (*-+>*)
(*<--*) :: (Integral a, Bounded a, Integral b, Bounded b) => b -> a -> ASM6502 ()
(*<--*) = flip (*-->*)


