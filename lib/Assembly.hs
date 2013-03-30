
module Assembly (
    Assembly(..), assembly_start, assembly_size, assembly_end, assembly_result, assembly_return,
    Assembler(..), assembler_function, assemble,
    section, nothing, here, unit_assembler,
    pad_assembler, return_assembler, fail_assembler, append_assembler, bind_assembler,
    enforce_counter, trace_counter,
    HasArea(..)
) where

import Data.Int
import Data.Word
import Data.Monoid
import qualified Data.Foldable as F
import Control.Monad.Fix
import Text.Printf
import Debug.Trace

 -- Yes, a and ctr are reversed in these two types.
 -- The reason is Assembly needs to be HasArea and Assembler needs to be Monad
data Assembly mon a ctr = Assembly ctr ctr mon (Maybe mon) (Maybe a)
assembly_start (Assembly x _ _ _ _) = x
assembly_size (Assembly s e _ _ _) = e - s
assembly_end (Assembly _ x _ _ _) = x
assembly_contents (Assembly _ _ x _ _) = x
assembly_result (Assembly _ _ _ (Just x) _) = x
assembly_result (Assembly _ _ _ Nothing _) = error$ "assembly_result called on an assembly that didn't have an associated result (e.g. one that was created from 'section')."
assembly_return (Assembly _ _ _ _ (Just x)) = x
assembly_return (Assembly _ _ _ _ Nothing) = error$ "assembly_return called on an assembly that didn't have an associated return value (e.g. one that was converted from an Integer)."

newtype Assembler mon ctr a = Assembler (ctr -> (ctr, mon, a))
assembler_function (Assembler f) = f

 -- This can be called like "assembly 0 ..." because there's a Num Assembly instance.
assemble :: (Monoid mon, Num ctr) => Assembly mon a ctr -> Assembler mon ctr b -> Assembly mon b ctr
assemble prev (Assembler f) = let
    (re, rp, rr) = f (assembly_end prev)
    in Assembly (assembly_end prev) re rp (Just (assembly_result prev <> rp)) (Just rr)

section :: (Monoid mon, Num ctr) => Assembler mon ctr a -> Assembler mon ctr (Assembly mon a ctr)
section (Assembler inner) = Assembler outer where
    outer pos = let
        (ie, ip, ir) = inner pos
        in (ie, ip, Assembly pos ie ip Nothing (Just ir))

nothing :: (Monoid mon, Num ctr) => Assembler mon ctr ()
nothing = Assembler f where f pos = (pos, mempty, ())

here :: Monoid mon => Assembler mon ctr ctr
here = Assembler f where f pos = (pos, mempty, pos)

unit_assembler :: Num ctr => mon -> Assembler mon ctr ()
unit_assembler x = Assembler f where f pos = (pos + 1, x, ())

return_assembler :: Monoid mon => a -> Assembler mon ctr a
return_assembler x = Assembler f where f pos = (pos, mempty, x)

fail_assembler :: Integral ctr => String -> Assembler mon ctr a
fail_assembler mess = Assembler f where  -- Be as lazy as possible.
    f pos = let
        err = error$ printf "%s at 0x%x" mess (toInteger pos)
        in (pos, err, err)

append_assembler :: Monoid mon => Assembler mon ctr a -> Assembler mon ctr b -> Assembler mon ctr b
append_assembler (Assembler left) (Assembler right) = Assembler f where
    f pos = let
        (le, lp, lr) = left pos
        (re, rp, rr) = right (le)
        in (re, lp <> rp, rr)

bind_assembler :: Monoid mon => Assembler mon ctr a -> (a -> Assembler mon ctr b) -> Assembler mon ctr b
bind_assembler (Assembler left) rightf = Assembler f where
    f pos = let
        (le, lp, lr) = left pos
        (re, rp, rr) = assembler_function (rightf lr) le
        in (re, lp <> rp, rr)

fix_assembler :: (a -> Assembler mon ctr a) -> Assembler mon ctr a
fix_assembler f = Assembler g where
    g pos = let
        (end, pay, ret) = assembler_function (f ret) pos
        in (end, pay, ret)

pad_assembler :: (Monoid mon, Integral ctr) => ctr -> mon -> Assembler mon ctr a -> Assembler mon ctr a
pad_assembler size filling (Assembler inner) = Assembler f where
    f pos = let
        (ie, ip, ir) = inner pos
        payload = if ie > pos + size
            then error$ printf "Code given to pad_assembler was larger than the alloted size (0x%x - 0x%x > 0x%x)"
                               (toInteger ie) (toInteger pos) (toInteger size)
            else ip <> F.fold (replicate (fromIntegral (pos + size - ie)) filling)
        in (pos + size, payload, ir)

enforce_counter :: (Monoid mon, Integral ctr) => ctr -> Assembler mon ctr ()
enforce_counter expected = Assembler f where
    f got = let
        payload = if got == expected
            then mempty
            else error$ printf "Something was misaligned (0x%x /= 0x%x)" (toInteger got) (toInteger expected)
        in (expected, payload, ())

trace_counter :: (Monoid mon, Integral ctr, Show a) => a -> Assembler mon ctr ()
trace_counter label = do
    spot <- here
    trace (show label ++ ": " ++ show (toInteger spot)) nothing

instance (Monoid mon, Integral ctr) => Monad (Assembler mon ctr) where
    return = return_assembler
    (>>=) = bind_assembler
    (>>) = append_assembler
    fail = fail_assembler

instance (Monoid mon, Integral ctr) => MonadFix (Assembler mon ctr) where
    mfix = fix_assembler

class HasArea a where
    start :: Num i => a i -> i
    end :: Num i => a i -> i
    size :: Num i => a i -> i

instance HasArea (Assembly mon a) where
    start = assembly_start
    end = assembly_end
    size = assembly_size

instance (Monoid mon, Num ctr) => Num (Assembly mon a ctr) where
    a + b = Assembly (start a + start b) (start a + start b) mempty (Just mempty) Nothing
    a - b = Assembly (start a - start b) (start a + start b) mempty (Just mempty) Nothing
    (*) = error$ "Can't (*) Assembly."
    abs = error$ "Can't abs Assembly."
    signum = error$ "Can't signum Assembly."
    fromInteger x = Assembly (fromInteger x) (fromInteger x) mempty (Just mempty) Nothing

instance (Monoid mon, Num ctr, Enum ctr) => Enum (Assembly mon a ctr) where
    succ a = Assembly (succ (start a)) (succ (start a)) mempty (Just mempty) Nothing
    pred a = Assembly (pred (start a)) (succ (start a)) mempty (Just mempty) Nothing
    toEnum = error$ "Can't toEnum Assembly."
    fromEnum = error$ "Can't fromEnum Assembly (you can toInteger it though)."

instance Eq ctr => Eq (Assembly mon a ctr) where
    a == b = assembly_start a == assembly_start b

instance Ord ctr => Ord (Assembly mon a ctr) where
    compare a b = compare (assembly_start a) (assembly_start b)

instance (Monoid mon, Real ctr) => Real (Assembly mon a ctr) where
    toRational = toRational . assembly_start

instance (Monoid mon, Integral ctr) => Integral (Assembly mon a ctr) where
    quotRem = error$ "Assembly is only Integral for its toInteger method.  You can't call quotRem and such on it.  Sory."
    toInteger = toInteger . assembly_start

instance (Monoid mon, Bounded ctr) => Bounded (Assembly mon a ctr) where
    minBound = Assembly minBound minBound mempty (Just mempty) Nothing
    maxBound = Assembly maxBound maxBound mempty (Just mempty) Nothing
