
{-# LANGUAGE DeriveDataTypeable, RecursiveDo #-}

module Assembler (
    Assemblage, assemblage_annotations, assemblage_start, assemblage_end, assemblage_result,
    Assembler(..), assembler_function, assemble,
    nothing, here, unit_assembler, return_assembler,
    fail_assembler, fail_assembler_if, generate_fail_message,
    append_assembler, bind_assembler, fix_assembler, pad_assembler,
    enforce_counter, trace_counter,
    Annotations, annotations_get, get_all_annotations,
    get_annotation, get_annotation_typed, get_annotation_default,
    set_annotation, clear_annotation, set_annotation_maybe,
    modify_annotation, with_annotation,
    Section(..), section_annotations, section_name, section_start, section_size, section_end, section_return,
    start, size, end,
    section, sect, allocate, allocate_named, allocate1, allocate1_named, provide,
    section_merge, appendable_section_name, get_section, current_section
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

 -- ASSEMBLAGES
data Assemblage mon ctr = Assemblage Annotations ctr ctr mon
assemblage_annotations (Assemblage x _ _ _) = x
assemblage_start (Assemblage _ x _ _) = x
assemblage_end (Assemblage _ _ x _) = x
assemblage_result (Assemblage _ _ _ x) = x

 -- This is so you can say "assemble 0 $ do ..."
instance (Monoid mon, Num ctr) => Num (Assemblage mon ctr) where
    a + b = Assemblage M.empty (assemblage_start a + assemblage_start b) (assemblage_start a + assemblage_start b) mempty
    a - b = Assemblage M.empty (assemblage_start a - assemblage_start b) (assemblage_start a - assemblage_start b) mempty
    a * b = Assemblage M.empty (assemblage_start a * assemblage_start b) (assemblage_start a * assemblage_start b) mempty
    abs = error$ "Cannot take the abs of an Assemblage."
    signum = error$ "Cannot take the signum of an Assemblage."
    fromInteger x = Assemblage M.empty (fromInteger x) (fromInteger x) mempty

 -- ASSEMBLERS
newtype Assembler mon ctr a = Assembler ((Annotations, ctr) -> (Annotations, ctr, mon, a))
assembler_function (Assembler f) = f

 -- Run an assembler and give its return value and an Assemblage
assemble :: (Monoid mon, Num ctr) => Assemblage mon ctr -> Assembler mon ctr a -> (a, Assemblage mon ctr)
assemble prev (Assembler f) = let
    (ann, re, rp, rr) = f (M.empty, assemblage_end prev)
    in (rr, Assemblage ann (assemblage_start prev) re (assemblage_result prev <> rp))

 -- An assembler that does nothing, more useful than you'd think.
nothing :: (Monoid mon, Num ctr) => Assembler mon ctr ()
nothing = Assembler f where f (ann, pos) = (ann, pos, mempty, ())

 -- Returns the current counter.  Use this to declare labels.
here :: Monoid mon => Assembler mon ctr ctr
here = Assembler f where f (ann, pos) = (ann, pos, mempty, pos)

 -- One of whatever mon is.
unit_assembler :: Num ctr => mon -> Assembler mon ctr ()
unit_assembler x = Assembler f where f (ann, pos) = (ann, pos + 1, x, ())

return_assembler :: Monoid mon => a -> Assembler mon ctr a
return_assembler x = Assembler f where f (ann, pos) = (ann, pos, mempty, x)

fail_assembler :: Integral ctr => String -> Assembler mon ctr a
fail_assembler mess = Assembler f where
    f (ann, pos) = let
        err = error$ printf "%s%s at 0x%x" mess (appendable_section_name ann) (toInteger pos)
        in (ann, pos, err, err)

 -- This is for being as lazy as possible
fail_assembler_if :: (Monoid mon, Integral ctr) => Bool -> String -> Assembler mon ctr ()
fail_assembler_if cond mess = Assembler f where
    f (ann, pos) = let
        err = error$ printf "%s%s at 0x%x" mess (appendable_section_name ann) (toInteger pos)
        in (ann, pos, if cond then err else mempty, ())

 -- If you need to put the fail message somewhere else
generate_fail_message :: (Monoid mon, Integral ctr) => String -> Assembler mon ctr String
generate_fail_message mess = Assembler f where
    f (ann, pos) = let
        message = printf "%s%s at 0x%x" mess (appendable_section_name ann) (toInteger pos)
        in (ann, pos, mempty, message)

 -- (>>)
append_assembler :: Monoid mon => Assembler mon ctr a -> Assembler mon ctr b -> Assembler mon ctr b
append_assembler (Assembler left) (Assembler right) = Assembler f where
    f (ann1, pos) = let
        (ann2, le, lp, lr) = left (ann1, pos)
        (ann3, re, rp, rr) = right (ann2, le)
        in (ann3, re, lp <> rp, rr)

 -- (>>=)
bind_assembler :: Monoid mon => Assembler mon ctr a -> (a -> Assembler mon ctr b) -> Assembler mon ctr b
bind_assembler (Assembler left) rightf = Assembler f where
    f (ann1, pos) = let
        (ann2, le, lp, lr) = left (ann1, pos)
        (ann3, re, rp, rr) = assembler_function (rightf lr) (ann2, le)
        in (ann3, re, lp <> rp, rr)

 -- mfix
fix_assembler :: (a -> Assembler mon ctr a) -> Assembler mon ctr a
fix_assembler f = Assembler g where
    g (ann1, pos) = let
        (ann2, end, pay, ret) = assembler_function (f ret) (ann1, pos)
        in (ann2, end, pay, ret)

 -- Force an section to be a certain size by padding it up.  This can break infinite dependency loops.
pad_assembler :: (Monoid mon, Integral ctr) => ctr -> mon -> Assembler mon ctr a -> Assembler mon ctr a
pad_assembler size filling (Assembler inner) = Assembler f where
    f (ann1, pos) = let
        (ann2, ie, ip, ir) = inner (ann1, pos)
        payload = if ie > pos + size
            then error$ printf "Code given to pad_assembler was larger than the alloted size%s (0x%x - 0x%x > 0x%x)"
                               (appendable_section_name ann2) (toInteger ie) (toInteger pos) (toInteger size)
            else ip <> F.fold (replicate (fromIntegral (pos + size - ie)) filling)
        in (ann2, pos + size, payload, ir)

 -- Make sure the counter is at a certain spot.  Used in, for instance, provide
enforce_counter :: (Monoid mon, Integral ctr) => ctr -> Assembler mon ctr ()
enforce_counter expected = Assembler f where
    f (ann, got) = let
        payload = if got == expected
            then mempty
            else error$ printf "Something was misaligned%s (0x%x /= 0x%x)"
                               (appendable_section_name ann) (toInteger got) (toInteger expected)
        in (ann, expected, payload, ())

 -- Debug.Trace.trace the current counter value
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

 -- ANNOTATIONS
 -- These provide
type Annotations = M.Map TypeRep Unknown
data Unknown
annotations_get :: Typeable a => Annotations -> Maybe a
annotations_get = f' undefined where
    f' :: Typeable a => a -> Annotations -> Maybe a
    f' undef = unsafeCoerce . M.lookup (typeOf undef)

 -- Get all the annotations at once.  You probably don't need to do this.
get_all_annotations :: Monoid mon => Assembler mon ctr Annotations
get_all_annotations = Assembler f where
    f (ann, pos) = (ann, pos, mempty, ann)

 -- Set an annotation to be a value.
set_annotation :: (Typeable a, Monoid mon) => a -> Assembler mon ctr ()
set_annotation x = set_annotation_maybe (Just x)

 -- Clear an annotation.  This takes the type from its argument and ignores the value,
 --  so you can pass it undefined or whatever.
clear_annotation :: (Typeable a, Monoid mon) => a -> Assembler mon ctr ()
clear_annotation x = Assembler f where
    f (ann, pos) = (M.delete (typeOf x) ann, pos, mempty, ())

 -- Set or clear an annotation based on the argument's Justice
set_annotation_maybe :: (Typeable a, Monoid mon) => Maybe a -> Assembler mon ctr ()
set_annotation_maybe x = Assembler f where
    ann2 ann = case x of
        Just v -> M.insert (typeOf v) (unsafeCoerce v) ann
        Nothing -> M.delete (typeOf (fromJust x)) ann
    f (ann, pos) = (ann2 ann, pos, mempty, ())

 -- Get Just an annotation or Nothing if it hasn't been set (or has been cleared)
get_annotation :: (Typeable a, Monoid mon) => Assembler mon ctr (Maybe a)
get_annotation = get_annotation_typed undefined

 -- In case you need to specify the type of the annotation.
get_annotation_typed :: (Typeable a, Monoid mon) => a -> Assembler mon ctr (Maybe a)
get_annotation_typed whatever = Assembler f where
    f (ann, pos) = (ann, pos, mempty, annotations_get ann)

 -- Get an annotation or a default value if it hasn't been set
get_annotation_default :: (Typeable a, Monoid mon) => a -> Assembler mon ctr a
get_annotation_default def = Assembler f where
    f (ann, pos) = (ann, pos, mempty, fromMaybe def (annotations_get ann))

 -- Modify an annotation in place.  If the annotation hasn't been set, the given default value
 --  will be process with the given function instead of the current value.
modify_annotation :: (Typeable a, Monoid mon, Integral ctr) => a -> (a -> a) -> Assembler mon ctr ()
modify_annotation def f = do
    ann <- get_annotation_default def
    set_annotation (Just (f ann))

 -- Temporarily set an annotation and restore its previous value after the block is done
with_annotation :: (Typeable a, Monoid mon, Integral ctr) => Maybe a -> Assembler mon ctr b -> Assembler mon ctr b
with_annotation x inner = do
    old <- get_annotation_typed (fromJust x)
    set_annotation x
    ret <- inner
    set_annotation old
    return ret

 -- SECTIONS
 -- A section describes simply a region of memory, either one containing code or
 --  one in RAM.  It may have a name, it may have annotations, and it may have
 --  a return value, but it always has a start and end.

data Section ctr a = Section Annotations String ctr ctr a
 -- Sections may carry Annotations in case they're needed.
 -- The set of annotations is that which was in effect when the section ended.
section_annotations :: Section ctr a -> Annotations
section_annotations (Section x _ _ _ _) = x
 -- The name of a section is primarily for diagnostic purposes, i.e.
 --  if there's an error in the code somewhere, the error message can
 --  tell you what section it is in.
section_name :: Section ctr a -> String
section_name (Section _ x _ _ _) = x
section_start :: Section ctr a -> ctr
section_start (Section _ _ x _ _) = x
start :: Section ctr a -> ctr
start = section_start
section_size :: Num ctr => Section ctr a -> ctr
section_size (Section _ _ s e _) = e - s
size :: Num ctr => Section ctr a -> ctr
size = section_size
section_end :: Section ctr a -> ctr
section_end (Section _ _ _ x _) = x
end :: Section ctr a -> ctr
end = section_end
 -- If you wanted to return something from an Assembler monad but it's buried in a Section, use this
section_return :: Section ctr a -> a
section_return (Section _ _ _ _ x) = x
 -- If the return type is bogging you down you can do this.
section_erase_return :: Section ctr a -> Section ctr Unknown
section_erase_return = unsafeCoerce
section_erase_types :: Section ctr a -> Section Unknown Unknown
section_erase_types = unsafeCoerce
section_recover_ctr_type :: Section Unknown Unknown -> Section ctr Unknown
section_recover_ctr_type = unsafeCoerce

 -- Various annotations to be used with sections
newtype NamedSections = NamedSections (M.Map String [Section Unknown Unknown]) deriving (Typeable)
newtype CurrentSection = CurrentSection (Section Unknown Unknown) deriving (Typeable)

 -- Wraps the code in an unnamed section
section :: (Monoid mon, Integral ctr) => Assembler mon ctr a -> Assembler mon ctr (Section ctr a)
section = sect ""

 -- Wraps the code in a named section, prepending it with the outer section name and "/"
sect :: (Monoid mon, Integral ctr) => String -> Assembler mon ctr a -> Assembler mon ctr (Section ctr a)
sect name inner = mdo
    let newname = if null name then name else fromMaybe name $ do  -- Maybe monad
            CurrentSection oldsection <- old
            return (section_name oldsection ++ "/" ++ name)
        sect = Section ann newname start end ret
        add_named_section (NamedSections map) = case newname of
            "" -> NamedSections map
            newname -> NamedSections (M.insertWith (++) newname [section_erase_types sect] map)
    old <- get_annotation
    set_annotation (CurrentSection (section_erase_types sect))
    modify_annotation (NamedSections M.empty) add_named_section
    start <- here
    ret <- inner
    end <- here
    ann <- get_all_annotations
    set_annotation_maybe old
    return sect


 -- Given a starting spot and a list of sizes, returns a list of sections.
 -- One use case for this is to allow separate banks in bank-switching systems to
 -- share a common layout.
allocate :: (Num ctr, Integral siz) => ctr -> [siz] -> [Section ctr ()]
allocate prev [] = []
allocate prev (z:zs) = let
    s = Section M.empty "" prev (prev + fromIntegral z) ()
    in s : allocate (end s) zs
 -- Like above but give the sections names as well.
allocate_named :: (Num ctr, Integral siz) => ctr -> [(String, siz)] -> [Section ctr ()]
allocate_named prev [] = []
allocate_named prev ((n,z):nzs) = let
    s = Section M.empty n prev (prev + fromIntegral z) ()
    in s : allocate_named (end s) nzs

 -- Like allocate but just make one Section.
allocate1 :: (Num ctr, Integral siz) => ctr -> siz -> Section ctr ()
allocate1 prev z = Section M.empty "" prev (prev + fromIntegral z) ()

 -- Like allocate1 but gives it a name.
allocate1_named :: (Num ctr, Integral siz) => String -> ctr -> siz -> Section ctr ()
allocate1_named name prev z = Section M.empty name prev (prev + fromIntegral z) ()

 -- Ensure that the given code satisfies the given section.
 --  It also propagates the name of the allocated section into the code.
 --  This is strict in the Maybe-ness of the name of the allocated section, because to
 --   be otherwise would just be a big pain.
provide :: (Monoid mon, Integral ctr) => Section ctr a -> Assembler mon ctr b -> Assembler mon ctr (Section ctr b)
provide allocation code = do
    enforce_counter (start allocation)
    ret <- sect (section_name allocation) code
    enforce_counter (end allocation)
    return ret

 -- Merge adjacent (allocated) Sections together.  Will fail if they're not adjacent.
section_merge :: Integral ctr => Section ctr a -> Section ctr b -> Section ctr b
section_merge a b = if end a == start b
    then Section M.empty "" (start a) (end b) (section_return b)
    else error$ printf "Tried to merge nonadjacent sections (0x%x..0x%x and 0x%x..0x%x)"
        (toInteger (start a)) (toInteger (end a)) (toInteger (start b)) (toInteger (end b))

 -- Get the name of the current section in a format that's appropriate for error messages
appendable_section_name :: Annotations -> String
appendable_section_name ann = case annotations_get ann of
    Just (CurrentSection sec) -> case section_name sec of
        "" -> ""
        name -> " in \"" ++ name ++ "\""
    Nothing -> ""

 -- Returns the section currently being processed.
current_section :: (Monoid mon, Integral ctr) => Assembler mon ctr (Maybe (Section ctr Unknown))
current_section = do
    x <- get_annotation
    let ret = case x of
            Just (CurrentSection sect) -> Just (section_recover_ctr_type sect)
            Nothing -> Nothing
    return ret

 -- Get a section with a certain name from an assemblage.  Will fail if no or multiple sections
 --  were given that name.
get_section :: String -> Assemblage mon ctr -> Section ctr Unknown
get_section name asg = case annotations_get (assemblage_annotations asg) of
    Just (NamedSections sections) -> case M.lookup name sections of
        Just [section] -> (section_recover_ctr_type section)
        Just (section:_) -> error$ "Can't get_section \"" ++ name ++ "\" because multiple sections with that name were assembled."
        _ -> error$ "Can't get_section \"" ++ name ++ "\" because no section with that name was assembled."
    Nothing -> error$ "Can't get_section \"" ++ name ++ "\" because no section with that name was assembled.  In fact, no named sections were assembled at all."

 -- Sections have to be able to act like integers.
instance Num ctr => Num (Section ctr a) where
    a + b = Section M.empty "" (start a + start b) (start a + start b) (noreturn "(+)")
    a - b = Section M.empty "" (start a - start b) (start a + start b) (noreturn "(-)")
    (*) = cantXsection "(*)"
    abs = cantXsection "abs"
    signum = cantXsection "signum"
    fromInteger x = Section M.empty "" (fromInteger x) (fromInteger x) (noreturn "fromInteger")


instance Enum ctr => Enum (Section ctr a) where
    succ a = Section M.empty "" (succ (start a)) (succ (start a)) (noreturn "succ")
    pred a = Section M.empty "" (pred (start a)) (pred (start a)) (noreturn "pred")
    toEnum = error$ "Can't toEnum to get a Section"
    fromEnum = error$ "Can't fromEnum Section (you can toInteger it though)"

instance Eq ctr => Eq (Section ctr a) where
    a == b = start a == start b

instance Ord ctr => Ord (Section ctr a) where
    compare a b = compare (start a) (start b)

instance Real ctr => Real (Section ctr a) where
    toRational = toRational . start

instance Integral ctr => Integral (Section ctr a) where
    quot = cantXsection "quot"
    rem = cantXsection "rem"
    div = cantXsection "div"
    mod = cantXsection "mod"
    quotRem = cantXsection "quotRem"
    divMod = cantXsection "divMod"
    toInteger = toInteger . start

instance Bounded ctr => Bounded (Section ctr a) where
    minBound = Section M.empty "" minBound minBound (noreturn "minBound")
    maxBound = Section M.empty "" maxBound maxBound (noreturn "maxBound")

cantXsection name = error $ "Can't " ++ name ++ " Section."
noreturn name = error $ "Section generated from " ++ name ++ " has no section_return."
