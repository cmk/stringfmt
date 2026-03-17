{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Prop.Layout (tests) where

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

opts80 :: LayoutOptions
opts80 = opts 80

short :: String -> Tree String ()
short s = leaf (length s) s

---------------------------------------------------------------------
-- P91–P110: Layout
---------------------------------------------------------------------

-- P91: pretty of a leaf is the content
prop_P91_leaf :: Property
prop_P91_leaf = property $ do
    s <- forAll $ Gen.string (Range.linear 1 30) Gen.alpha
    pretty opts80 (short s) === s

-- P92: pretty of cat is concatenation
prop_P92_cat :: Property
prop_P92_cat = property $ do
    s <- forAll $ Gen.string (Range.linear 1 10) Gen.alpha
    t <- forAll $ Gen.string (Range.linear 1 10) Gen.alpha
    pretty opts80 (short s <> short t) === s ++ t

-- P93: hardline produces a newline
prop_P93_hardline :: Property
prop_P93_hardline = property $ do
    let result = pretty opts80 (short "a" <> hardline <> short "b")
    result === "a\nb"

-- P94: nest indents after line break
prop_P94_nest :: Property
prop_P94_nest = property $ do
    let result = pretty opts80 (short "a" <> nest 4 (hardline <> short "b"))
    result === "a\n    b"

-- P95: group fits on one line when width allows
prop_P95_group_fits :: Property
prop_P95_group_fits = property $ do
    let doc = group (short "hello" <> line <> short "world")
    pretty (opts 80) doc === "hello world"

-- P96: group breaks when content exceeds width
prop_P96_group_breaks :: Property
prop_P96_group_breaks = property $ do
    let doc = group (short "hello" <> line <> short "world")
    pretty (opts 5) doc === "hello\nworld"

-- P97: emptyDoc renders as ""
prop_P97_empty :: Property
prop_P97_empty = property $ do
    pretty opts80 (emptyDoc :: Tree String ()) === ""

-- P98: flatAlt uses default in non-flat context
prop_P98_flatalt :: Property
prop_P98_flatalt = property $ do
    let doc = flatAlt (short "default") (short "flat")
    pretty opts80 doc === "default"

-- P99: layoutCompact always breaks
prop_P99_compact :: Property
prop_P99_compact = property $ do
    let doc = group (short "hello" <> line <> short "world")
        result = render (layoutCompact doc)
    result === "hello\nworld"

-- P100: Fail in flattened branch causes fallback
prop_P100_fail_fallback :: Property
prop_P100_fail_fallback = property $ do
    -- group tries to flatten; hardline becomes Fail; falls back
    let doc = group (short "a" <> hardline <> short "b")
    pretty (opts 80) doc === "a\nb"

-- P101: token stream preserves content
prop_P101_token_content :: Property
prop_P101_token_content = property $ do
    s <- forAll $ Gen.string (Range.linear 1 20) Gen.alpha
    let tokens = layoutPretty opts80 (short s)
    tokens === [TLeaf (length s) s]

-- P102: TLine renders as newline + spaces
prop_P102_tline :: Property
prop_P102_tline = property $ do
    n <- forAll $ Gen.int (Range.linear 0 10)
    render [TLine n :: Token String ()] === "\n" ++ replicate n ' '

-- P103: annotations are balanced
prop_P103_ann_balanced :: Property
prop_P103_ann_balanced = property $ do
    let doc = T.annotate () (short "hello")
        tokens = layoutPretty opts80 doc
        pushes = length [() | TAnnPush _ <- tokens]
        pops = length [() | TAnnPop <- tokens]
    pushes === pops

-- P104: annotations don't affect rendered text
prop_P104_ann_transparent :: Property
prop_P104_ann_transparent = property $ do
    s <- forAll $ Gen.string (Range.linear 1 20) Gen.alpha
    let doc = T.annotate () (short s)
    pretty opts80 doc === s

-- P105: align aligns to current column
prop_P105_align :: Property
prop_P105_align = property $ do
    let doc = short "key: " <> align (short "line1" <> hardline <> short "line2")
    pretty opts80 doc === "key: line1\n     line2"

-- P106: nested group picks best layout at each level
prop_P106_nested_group :: Property
prop_P106_nested_group = property $ do
    let inner = group (short "a" <> line <> short "b")
        outer = group (short "(" <> inner <> short ")")
    -- Wide enough: all on one line
    pretty (opts 80) outer === "(a b)"
    -- Too narrow for everything: outer breaks but inner fits
    pretty (opts 5) outer === "(a b)"
    -- Very narrow: both break
    pretty (opts 3) outer === "(a\nb)"

-- P107: Unbounded always picks single-line
prop_P107_unbounded :: Property
prop_P107_unbounded = property $ do
    let doc = group (short "hello" <> line <> short "world")
    pretty (LayoutOptions Unbounded) doc === "hello world"

-- P108: nest 0 is identity
prop_P108_nest_zero :: Property
prop_P108_nest_zero = property $ do
    let doc = short "a" <> nest 0 (hardline <> short "b")
    pretty opts80 doc === "a\nb"

-- P109: line' becomes empty when flattened
prop_P109_line_prime :: Property
prop_P109_line_prime = property $ do
    let doc = group (short "a" <> line' <> short "b")
    pretty (opts 80) doc === "ab"

-- P110: multiple groups with varying widths
prop_P110_multi_group :: Property
prop_P110_multi_group = property $ do
    let item s = group (short s <> line <> short s)
        doc = item "xx" <> line <> item "yy"
    -- Wide: all flat
    pretty (opts 80) doc === "xx xx\nyy yy"
    -- Narrow: items break
    pretty (opts 4) doc === "xx\nxx\nyy\nyy"
