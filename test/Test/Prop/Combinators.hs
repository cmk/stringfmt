{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Prop.Combinators (tests) where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Data.Fmt.Functor (Tree)
import Data.Fmt.Tree hiding (annotate)
import qualified Data.Fmt.Tree as T
import Data.String (fromString)

tests :: IO Bool
tests = checkParallel $$(discover)

---------------------------------------------------------------------
-- Helpers
---------------------------------------------------------------------

opts :: Int -> LayoutOptions
opts w = LayoutOptions (AvailablePerLine w 1.0)

short :: String -> Tree String ann
short s = leaf (length s) s

pr :: Int -> Tree String ann -> String
pr w = pretty (opts w)

pr80 :: Tree String ann -> String
pr80 = pr 80

---------------------------------------------------------------------
-- P111–P140: Derived combinators
---------------------------------------------------------------------

-- P111: hsep joins with spaces
prop_P111_hsep :: Property
prop_P111_hsep = property $ do
    pr80 (hsep [short "a", short "b", short "c"]) === "a b c"

-- P112: vsep joins with newlines
prop_P112_vsep :: Property
prop_P112_vsep = property $ do
    pr80 (vsep [short "a", short "b", short "c"]) === "a\nb\nc"

-- P113: sep = group . vsep
prop_P113_sep :: Property
prop_P113_sep = property $ do
    let ds = [short "hello", short "world"]
    pr80 (sep ds) === pr80 (group (vsep ds))

-- P114: hsep [] = emptyDoc
prop_P114_hsep_empty :: Property
prop_P114_hsep_empty = property $ do
    pr80 (hsep ([] :: [Tree String ()])) === ""

-- P115: fillSep fills as many per line as fit
prop_P115_fillSep :: Property
prop_P115_fillSep = property $ do
    let ds = map (\c -> short [c]) ['a' .. 'f']
    -- Wide: all on one line
    pr80 (fillSep ds) === "a b c d e f"
    -- Narrow: breaks where needed
    pr 5 (fillSep ds) === "a b c\nd e f"

-- P116: x <+> y = x <> " " <> y flat
prop_P116_space :: Property
prop_P116_space = property $ do
    pr80 (short "a" <+> short "b") === "a b"

-- P117: hcat = concat without separators
prop_P117_hcat :: Property
prop_P117_hcat = property $ do
    pr80 (hcat [short "a", short "b", short "c"]) === "abc"

-- P118: vcat uses line'
prop_P118_vcat :: Property
prop_P118_vcat = property $ do
    pr80 (vcat [short "a", short "b"]) === "a\nb"

-- P119: hang i d = align (nest i d)
prop_P119_hang :: Property
prop_P119_hang = property $ do
    let d = short "hello" <> hardline <> short "world"
    pr80 (hang 4 d) === pr80 (align (nest 4 d))

-- P120: indent i inserts spaces then aligns
prop_P120_indent :: Property
prop_P120_indent = property $ do
    let d = vsep [short "line1", short "line2"]
    pr80 (indent 4 d) === "    line1\n    line2"

-- P121: indent 0 = align
prop_P121_indent_zero :: Property
prop_P121_indent_zero = property $ do
    let d = short "x"
    pr80 (indent 0 d) === pr80 (align d)

-- P122: nest (nest) = nest (sum)
prop_P122_nest_compose :: Property
prop_P122_nest_compose = property $ do
    let d = hardline <> short "x"
    pr80 (nest 2 (nest 3 d)) === pr80 (nest 5 d)

-- P123: list renders as [a, b]
prop_P123_list :: Property
prop_P123_list = property $ do
    pr80 (list [short "a", short "b"]) === "[a, b]"

-- P124: list breaks when doesn't fit
prop_P124_list_break :: Property
prop_P124_list_break = property $ do
    let result = pr 5 (list [short "aaa", short "bbb"])
    assert $ '\n' `elem` result

-- P125: tupled renders as (a, b)
prop_P125_tupled :: Property
prop_P125_tupled = property $ do
    pr80 (tupled [short "a", short "b"]) === "(a, b)"

-- P126: encloseSep with custom delimiters
prop_P126_encloseSep :: Property
prop_P126_encloseSep = property $ do
    let result = pr80 $ encloseSep (short "{") (short "}") (short "; ")
                                   [short "a", short "b", short "c"]
    assert $ 'a' `elem` result
    assert $ 'b' `elem` result
    assert $ 'c' `elem` result

-- P127: punctuate appends sep to all but last
prop_P127_punctuate :: Property
prop_P127_punctuate = property $ do
    let result = punctuate (short ",") [short "a", short "b", short "c"]
    pr80 (hcat result) === "a,b,c"

-- P128: fill pads to width
prop_P128_fill :: Property
prop_P128_fill = property $ do
    pr80 (fill 10 (short "hi") <> short "|") === "hi        |"

-- P129: fill does not pad if exceeds
prop_P129_fill_exceed :: Property
prop_P129_fill_exceed = property $ do
    pr80 (fill 2 (short "hello") <> short "|") === "hello|"

-- P130: fillBreak breaks after if exceeds
prop_P130_fillBreak :: Property
prop_P130_fillBreak = property $ do
    pr80 (fillBreak 2 (short "hello") <> short "|") === "hello\n  |"

-- P131: width passes rendered width
prop_P131_width :: Property
prop_P131_width = property $ do
    let d = width (short "hello") $ \w ->
                short (" (" ++ show w ++ ")")
    pr80 d === "hello (5)"

-- P132: surround
prop_P132_surround :: Property
prop_P132_surround = property $ do
    pr80 (surround (short "x") (short "[") (short "]")) === "[x]"

-- P133: reAnnotate id = id (on rendered output)
prop_P133_reAnnotate_id :: Property
prop_P133_reAnnotate_id = property $ do
    let d = T.annotate () (short "hello")
    pr80 (reAnnotate id d) === pr80 d

-- P134: reAnnotate compose
prop_P134_reAnnotate_compose :: Property
prop_P134_reAnnotate_compose = property $ do
    let d = T.annotate (1 :: Int) (short "hello")
        f = (+ 10)
        g = (* 2)
    pr80 (reAnnotate (f . g) d) === pr80 (reAnnotate f (reAnnotate g d))

-- P135: unAnnotate strips all
prop_P135_unAnnotate :: Property
prop_P135_unAnnotate = property $ do
    let d = T.annotate () (short "hello")
    pr80 (unAnnotate d) === "hello"

-- P136: unAnnotate . annotate = id on rendered output
prop_P136_unAnnotate_annotate :: Property
prop_P136_unAnnotate_annotate = property $ do
    pr80 (unAnnotate (T.annotate () (short "x"))) === pr80 (short "x")

-- P137: reAnnotate as hoist matches fold version
prop_P137_reAnnotate_hoist :: Property
prop_P137_reAnnotate_hoist = property $ do
    let d = T.annotate (1 :: Int) (short "hi" <> T.annotate 2 (short "lo"))
    pr80 (reAnnotate (+ 10) d) === pr80 (reAnnotate (+ 10) d)

-- P138: alterAnnotations pure = id
prop_P138_alterAnnotations_pure :: Property
prop_P138_alterAnnotations_pure = property $ do
    let d = T.annotate () (short "hello")
    pr80 (alterAnnotations (: []) d) === pr80 d

-- P139: annotation balance preserved by reAnnotate
prop_P139_ann_balance :: Property
prop_P139_ann_balance = property $ do
    let d = T.annotate (1 :: Int) (short "a" <> T.annotate 2 (short "b"))
        tokens = layoutPretty (opts 80) (reAnnotate (+ 10) d)
        pushes = length [() | TAnnPush _ <- tokens]
        pops = length [() | TAnnPop <- tokens]
    pushes === pops

-- P140: rendered text unchanged by reAnnotate
prop_P140_ann_content :: Property
prop_P140_ann_content = property $ do
    let d = T.annotate (1 :: Int) (short "hello")
    pr80 d === pr80 (reAnnotate (+ 10) d)
