
{-# LANGUAGE DeriveDataTypeable #-}

module Assembler (
    Section(..), section_start, section_size, section_end, section_result, section_return,
    start, size, end,
    Assembler(..), assembler_function, assemble, allocate, allocate1,
    section, nothing, here, unit_assembler,
    pad_assembler, return_assembler, fail_assembler, append_assembler, bind_assembler,
    enforce_counter, provide, trace_counter,
    get_annotation, get_annotation_default, set_annotation,
    area, assemble_area, get_area, appendable_area_name, current_area,
    section_merge
) where

import Data.Int
import Data.Word
import Data.Monoid
import Data.Typeable
import Unsafe.Coerce
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Foldable as F
import Control.Monad.Fix
import Text.Printf
import Debug.Trace

data Unknown
type Annotations = M.Map TypeRep Unknown
annotations_get :: Typeable a => Annotations -> Maybe a
annotations_get = f' undefined where
    f' :: Typeable a => a -> Annotations -> Maybe a
    f' undef = unsafeCoerce . M.lookup (typeOf undef)

data Section mon ctr a = Section Annotations ctr ctr mon mon a
section_annotations :: Section mon ctr a -> Annotations
section_annotations (Section x _ _ _ _ _) = x
section_start :: Section mon ctr a -> ctr
section_start (Section _ x _ _ _ _) = x
start :: Section mon ctr a -> ctr
start = section_start
section_size :: Num ctr => Section mon ctr a -> ctr
section_size (Section _ s e _ _ _) = e - s
size :: Num ctr => Section mon ctr a -> ctr
size = section_size
section_end :: Section mon ctr a -> ctr
section_end (Section _ _ x _ _ _) = x
end :: Section mon ctr a -> ctr
end = section_end
section_contents :: Section mon ctr a -> mon
section_contents (Section _ _ _ x _ _) = x
section_result :: Section mon ctr a -> mon
section_result (Section _ _ _ _ x _) = x
section_return :: Section mon ctr a -> a
section_return (Section _ _ _ _ _ x) = x

nosomething something name = error$ "Section generated with " ++ name ++ " has no " ++ something
noannotations = nosomething "section_annotations"
nocontents = nosomething "section_contents"
noresult = nosomething "section_result"
noreturn = nosomething "section_return"
cantXsection x = error$ "Can't " ++ x ++ " Section.  Please extract its start first."

newtype Assembler mon ctr a = Assembler ((Annotations, ctr) -> (Annotations, ctr, mon, a))
assembler_function (Assembler f) = f

 -- This can be called like "section 0 ..." because there's a Num Section instance.
assemble :: (Monoid mon, Num ctr) => Section mon ctr a -> Assembler mon ctr b -> Section mon ctr b
assemble prev (Assembler f) = let
    (ann, re, rp, rr) = f (M.empty, section_end prev)
    in Section ann (section_end prev) re rp (section_result prev <> rp) rr

allocate :: (Num ctr, Integral siz) => Section mon ctr a -> [siz] -> [Section mon ctr b]
allocate prev [] = []
allocate prev (z:zs) = let
    s = Section (noannotations "allocate") (end prev) (end prev + fromIntegral z) (nocontents "allocate") (noresult "allocate") (noreturn "allocate")
    in s : allocate s zs

allocate1 :: (Num ctr, Integral siz) => Section mon ctr a -> siz -> Section mon ctr b
allocate1 prev z = Section (noannotations "allocate1") (end prev) (end prev + fromIntegral z) (nocontents "allocate1") (noresult "allocate1") (noreturn "allocate1")

section :: (Monoid mon, Num ctr) => Assembler mon ctr a -> Assembler mon ctr (Section mon ctr a)
section (Assembler inner) = Assembler outer where
    outer (ann1, pos) = let
        (ann2, ie, ip, ir) = inner (ann1, pos)
        in (ann2, ie, ip, Section ann2 pos ie ip (noresult "section") ir)

section_ :: (Monoid mon, Num ctr) => Assembler mon ctr a -> Assembler mon ctr (Section mon ctr ())
section_ (Assembler inner) = Assembler outer where
    outer (ann1, pos) = let
        (ann2, ie, ip, ir) = inner (ann1, pos)
        in (ann2, ie, ip, Section ann2 pos ie ip (noresult "section") ())

nothing :: (Monoid mon, Num ctr) => Assembler mon ctr ()
nothing = Assembler f where f (ann, pos) = (ann, pos, mempty, ())

here :: Monoid mon => Assembler mon ctr ctr
here = Assembler f where f (ann, pos) = (ann, pos, mempty, pos)

unit_assembler :: Num ctr => mon -> Assembler mon ctr ()
unit_assembler x = Assembler f where f (ann, pos) = (ann, pos + 1, x, ())

return_assembler :: Monoid mon => a -> Assembler mon ctr a
return_assembler x = Assembler f where f (ann, pos) = (ann, pos, mempty, x)

fail_assembler :: Integral ctr => String -> Assembler mon ctr a
fail_assembler mess = Assembler f where
    f (ann, pos) = let
        err = error$ printf "%s%s at 0x%x" mess (appendable_area_name ann) (toInteger pos)
        in (ann, pos, err, err)

 -- This is for being as lazy as possible
fail_assembler_if :: (Monoid mon, Integral ctr) => Bool -> String -> Assembler mon ctr ()
fail_assembler_if cond mess = Assembler f where
    f (ann, pos) = let
        err = error$ printf "%s%s at 0x%x" mess (appendable_area_name ann) (toInteger pos)
        in (ann, pos, if cond then err else mempty, ())

append_assembler :: Monoid mon => Assembler mon ctr a -> Assembler mon ctr b -> Assembler mon ctr b
append_assembler (Assembler left) (Assembler right) = Assembler f where
    f (ann1, pos) = let
        (ann2, le, lp, lr) = left (ann1, pos)
        (ann3, re, rp, rr) = right (ann2, le)
        in (ann3, re, lp <> rp, rr)

bind_assembler :: Monoid mon => Assembler mon ctr a -> (a -> Assembler mon ctr b) -> Assembler mon ctr b
bind_assembler (Assembler left) rightf = Assembler f where
    f (ann1, pos) = let
        (ann2, le, lp, lr) = left (ann1, pos)
        (ann3, re, rp, rr) = assembler_function (rightf lr) (ann2, le)
        in (ann3, re, lp <> rp, rr)

fix_assembler :: (a -> Assembler mon ctr a) -> Assembler mon ctr a
fix_assembler f = Assembler g where
    g (ann1, pos) = let
        (ann2, end, pay, ret) = assembler_function (f ret) (ann1, pos)
        in (ann2, end, pay, ret)

pad_assembler :: (Monoid mon, Integral ctr) => ctr -> mon -> Assembler mon ctr a -> Assembler mon ctr a
pad_assembler size filling (Assembler inner) = Assembler f where
    f (ann1, pos) = let
        (ann2, ie, ip, ir) = inner (ann1, pos)
        payload = if ie > pos + size
            then error$ printf "Code given to pad_assembler was larger than the alloted size%s (0x%x - 0x%x > 0x%x)"
                               (appendable_area_name ann2) (toInteger ie) (toInteger pos) (toInteger size)
            else ip <> F.fold (replicate (fromIntegral (pos + size - ie)) filling)
        in (ann2, pos + size, payload, ir)

enforce_counter :: (Monoid mon, Integral ctr) => ctr -> Assembler mon ctr ()
enforce_counter expected = Assembler f where
    f (ann, got) = let
        payload = if got == expected
            then mempty
            else error$ printf "Something was misaligned%s (0x%x /= 0x%x)"
                               (appendable_area_name ann) (toInteger got) (toInteger expected)
        in (ann, expected, payload, ())

provide :: (Monoid mon, Integral ctr) => Section mon ctr a -> Assembler mon ctr b -> Assembler mon ctr b
provide allocation code = do
    enforce_counter (start allocation)
    ret <- code
    enforce_counter (end allocation)
    return ret

trace_counter :: (Monoid mon, Integral ctr, Show a) => a -> Assembler mon ctr ()
trace_counter label = do
    spot <- here
    trace (show label ++ ": " ++ show (toInteger spot)) nothing

set_annotation :: (Typeable a, Monoid mon) => Maybe a -> Assembler mon ctr ()
set_annotation x = Assembler f where
    ann2 ann = case x of
        Just v -> M.insert (typeOf v) (unsafeCoerce v) ann
        Nothing -> M.delete (typeOf (fromJust x)) ann
    f (ann, pos) = (ann2 ann, pos, mempty, ())

get_annotation :: (Typeable a, Monoid mon) => Assembler mon ctr (Maybe a)
get_annotation = f' undefined where
    f' :: (Typeable a, Monoid mon) => a -> Assembler mon ctr (Maybe a)
    f' undef = Assembler f where
        f (ann, pos) = (ann, pos, mempty, annotations_get ann)

get_annotation_default :: (Typeable a, Monoid mon) => a -> Assembler mon ctr a
get_annotation_default def = Assembler f where
    f (ann, pos) = (ann, pos, mempty, fromMaybe def (annotations_get ann))

modify_annotation :: (Typeable a, Monoid mon, Integral ctr) => a -> (a -> a) -> Assembler mon ctr ()
modify_annotation def f = do
    ann <- get_annotation_default def
    set_annotation (Just (f ann))

newtype AllAreas = AllAreas (M.Map String [Section () () ()]) deriving (Typeable)
newtype CurrentArea = CurrentArea String deriving (Typeable)

area :: (Monoid mon, Integral ctr) => String -> Assembler mon ctr a -> Assembler mon ctr (Section mon ctr a)
area name code = do
    set_annotation (Just (CurrentArea name))
    sect <- section code
    set_annotation (Nothing :: Maybe CurrentArea)
    let unitsect = Section (section_annotations sect)
                           (section_start sect)
                           (section_end sect)
                           (section_contents sect)
                           (section_result sect)
                           ()
        insertsect = unsafeCoerce unitsect :: Section () () ()
        mod (AllAreas m) = AllAreas (M.insertWith (++) name [insertsect] m)
    modify_annotation (AllAreas M.empty) mod
    return sect

appendable_area_name :: Annotations -> String
appendable_area_name ann = case annotations_get ann of
    Just (CurrentArea name) -> " in area \"" ++ name ++ "\""
    Nothing -> ""

assemble_area :: (Monoid mon, Integral ctr) => Section mon ctr a -> String -> Assembler mon ctr b -> Section mon ctr b
assemble_area prev name code = assemble prev (area name code >>= return . section_return)

get_area :: Section mon ctr a -> String -> Section mon ctr ()
get_area sec name = case annotations_get (section_annotations sec) of
    Just (AllAreas areas) -> case M.lookup name areas of
        Just [area] -> unsafeCoerce area
        Just (area:_) -> error$ "Can't get_area \"" ++ name ++ "\" because multiple areas with that name were assembled."
        _ -> error$ "Can't get_area \"" ++ name ++ "\" because no area with that name was assembled."
    Nothing -> error$ "Can't get_area \"" ++ name ++ "\" because no area with that name was assembled."

current_area :: (Monoid mon, Integral ctr) => Assembler mon ctr (Maybe String)
current_area = do
    x <- get_annotation
    let ret = case x of
            Just (CurrentArea name) -> Just name
            Nothing -> Nothing
    return ret

instance (Monoid mon, Integral ctr) => Monad (Assembler mon ctr) where
    return = return_assembler
    (>>=) = bind_assembler
    (>>) = append_assembler
    fail = fail_assembler

instance (Monoid mon, Integral ctr) => MonadFix (Assembler mon ctr) where
    mfix = fix_assembler

instance (Monoid mon, Num ctr) => Num (Section mon ctr a) where
    a + b = Section (noannotations "(+)") (start a + start b) (start a + start b) mempty mempty (noreturn "(+)")
    a - b = Section (noannotations "(+)") (start a - start b) (start a + start b) mempty mempty (noreturn "(-)")
    (*) = cantXsection "(*)"
    abs = cantXsection "abs"
    signum = cantXsection "signum"
    fromInteger x = Section M.empty (fromInteger x) (fromInteger x) mempty mempty (noreturn "fromInteger")

instance (Monoid mon, Num ctr, Enum ctr) => Enum (Section mon ctr a) where
    succ a = Section (noannotations "succ") (succ (start a)) (succ (start a)) mempty mempty (noreturn "succ")
    pred a = Section (noannotations "pred") (pred (start a)) (pred (start a)) mempty mempty (noreturn "pred")
    toEnum = error$ "Can't toEnum Section"
    fromEnum = error$ "Can't fromEnum Section (you can toInteger it though)"

instance Eq ctr => Eq (Section mon ctr a) where
    a == b = section_start a == section_start b

instance Ord ctr => Ord (Section mon ctr a) where
    compare a b = compare (section_start a) (section_start b)

instance (Monoid mon, Real ctr) => Real (Section mon ctr a) where
    toRational = toRational . section_start

instance (Monoid mon, Integral ctr) => Integral (Section mon ctr a) where
    quot = cantXsection "quot"
    rem = cantXsection "rem"
    div = cantXsection "div"
    mod = cantXsection "mod"
    quotRem = cantXsection "quotRem"
    divMod = cantXsection "divMod"
    toInteger = toInteger . section_start

instance (Monoid mon, Bounded ctr) => Bounded (Section mon ctr a) where
    minBound = Section (noannotations "minBound") minBound minBound mempty mempty (noreturn "minBound")
    maxBound = Section (noannotations "maxBound") maxBound maxBound mempty mempty (noreturn "maxBound")

section_merge :: (Monoid mon, Integral ctr) => Section mon ctr a -> Section mon ctr b -> Section mon ctr b
section_merge a b = if end a == start b
    then Section (noannotations "section_merge") (start a) (end b) (section_contents a <> section_contents b) (noresult "section_merge") (section_return b)
    else error$ printf "Tried to merge nonadjacent sections (0x%x..0x%x and 0x%x..0x%x)"
        (toInteger (start a)) (toInteger (end a)) (toInteger (start b)) (toInteger (end b))
