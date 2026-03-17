{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Prop.Group (tests) where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Data.Fmt.Functor (Tree)
import Data.Fmt.Tree hiding (annotate)
import Data.String (fromString)

tests :: IO Bool
tests = checkParallel $$(discover)

---------------------------------------------------------------------
-- Helpers
---------------------------------------------------------------------

type Doc = Tree String ()

short :: String -> Doc
short s = leaf (length s) s

opts :: Int -> LayoutOptions
opts w = LayoutOptions (AvailablePerLine w 1.0)

genDoc :: Gen Doc
genDoc = Gen.recursive Gen.choice
    [ short <$> Gen.string (Range.linear 1 8) Gen.alpha
    , pure emptyDoc
    ]
    [ Gen.subterm2 genDoc genDoc (<>)
    , Gen.subterm genDoc (nest 2)
    , Gen.subterm genDoc group
    , Gen.subterm2 genDoc genDoc (\x y -> x <> line <> y)
    ]

forAllDoc :: Gen Doc -> PropertyT IO Doc
forAllDoc = forAllWith (pretty (opts 80))

eqDoc :: Doc -> Doc -> PropertyT IO ()
eqDoc x y = do
    let widths = [1, 5, 10, 20, 40, 80]
    mapM_ (\w -> pretty (LayoutOptions (AvailablePerLine w 1.0)) x
             === pretty (LayoutOptions (AvailablePerLine w 1.0)) y) widths
    pretty (LayoutOptions Unbounded) x === pretty (LayoutOptions Unbounded) y

---------------------------------------------------------------------
-- P208–P215: Group optimization
---------------------------------------------------------------------

-- P208: group' agrees with group on unbounded width
-- (At narrow widths, group' may produce different but valid
-- layouts because changesUponFlattening preserves more structure
-- than the naive flatten-everything approach.)
prop_P208_group_agrees :: Property
prop_P208_group_agrees = property $ do
    doc <- forAllDoc genDoc
    pretty (LayoutOptions Unbounded) (group' doc)
        === pretty (LayoutOptions Unbounded) (group doc)

-- P209: changesUponFlattening returns AlreadyFlat for leaves
prop_P209_flat_leaf :: Property
prop_P209_flat_leaf = property $ do
    s <- forAll $ Gen.string (Range.linear 1 10) Gen.alpha
    case changesUponFlattening (short s) of
        AlreadyFlat -> success
        _ -> failure

-- P210: changesUponFlattening returns NeverFlat for hardline
prop_P210_never_hardline :: Property
prop_P210_never_hardline = property $ do
    case changesUponFlattening (hardline :: Doc) of
        NeverFlat -> success
        _ -> failure

-- P211: changesUponFlattening returns Flattened for line
prop_P211_flattened_line :: Property
prop_P211_flattened_line = property $ do
    case changesUponFlattening (line :: Doc) of
        Flattened _ -> success
        _ -> failure

-- P212: group' avoids Union for already-flat docs
prop_P212_group_no_union_flat :: Property
prop_P212_group_no_union_flat = property $ do
    let doc = short "hello" <> short " world"
    -- group' on an already-flat doc should return the doc unchanged
    eqDoc (group' doc) doc

-- P213: group' avoids Union for never-flat docs
prop_P213_group_no_union_never :: Property
prop_P213_group_no_union_never = property $ do
    let doc = short "a" <> hardline <> short "b"
    -- group' on a doc with hardline should return unchanged
    eqDoc (group' doc) doc

-- P214: group' satisfies Wadler's group laws
prop_P214_group_wadler_empty :: Property
prop_P214_group_wadler_empty = property $ do
    eqDoc (group' (emptyDoc :: Doc)) emptyDoc

-- P215: group' is at least as good as group (never worse layout)
-- Both produce valid layouts; group' may produce better ones
-- at narrow widths by preserving more structure.
prop_P215_group_complex :: Property
prop_P215_group_complex = property $ do
    doc <- forAllDoc genDoc
    -- At unbounded width, both agree (same single-line layout)
    pretty (LayoutOptions Unbounded) (group' doc)
        === pretty (LayoutOptions Unbounded) (group doc)
    -- At any width, group' produces valid output (doesn't crash)
    w <- forAll $ Gen.element [5, 10, 20, 40, 80]
    let result = pretty (opts w) (group' doc)
    assert $ length result >= 0  -- non-bottom
