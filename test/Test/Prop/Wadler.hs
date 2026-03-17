{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Wadler's algebraic laws for pretty-printing combinators.
--
-- From: Philip Wadler, "A prettier printer", draft paper, April 1997,
-- revised March 1998.
-- https://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf
--
-- All laws are tested via rendering at multiple widths since
-- structural equality is not available for trees containing
-- 'Column'/'Nesting' functions.
module Test.Prop.Wadler (tests) where

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

-- | Test equality via rendering at multiple widths.
-- Two documents are considered equal if they render identically
-- at widths 1, 5, 10, 20, 40, 80, and unbounded.
eqDoc :: Doc -> Doc -> PropertyT IO ()
eqDoc x y = do
    let widths = [1, 5, 10, 20, 40, 80]
    mapM_ (\w -> pretty (LayoutOptions (AvailablePerLine w 1.0)) x
             === pretty (LayoutOptions (AvailablePerLine w 1.0)) y) widths
    pretty (LayoutOptions Unbounded) x === pretty (LayoutOptions Unbounded) y

genDoc :: Gen Doc
genDoc = Gen.recursive Gen.choice
    [ short <$> Gen.string (Range.linear 1 8) Gen.alpha
    , pure emptyDoc
    ]
    [ Gen.subterm2 genDoc genDoc (<>)
    , Gen.subterm genDoc (nest 2)
    , Gen.subterm genDoc group
    ]

forAllDoc :: Gen Doc -> PropertyT IO Doc
forAllDoc = forAllWith (pretty (LayoutOptions (AvailablePerLine 80 1.0)))

---------------------------------------------------------------------
-- Concatenation: <> is associative with empty as unit
---------------------------------------------------------------------

-- P173: x <> (y <> z) = (x <> y) <> z
prop_P173_cat_assoc :: Property
prop_P173_cat_assoc = property $ do
    x <- forAllDoc genDoc
    y <- forAllDoc genDoc
    z <- forAllDoc genDoc
    eqDoc (x <> (y <> z)) ((x <> y) <> z)

-- P174: x <> empty = x
prop_P174_cat_right_unit :: Property
prop_P174_cat_right_unit = property $ do
    x <- forAllDoc genDoc
    eqDoc (x <> emptyDoc) x

-- P175: empty <> x = x
prop_P175_cat_left_unit :: Property
prop_P175_cat_left_unit = property $ do
    x <- forAllDoc genDoc
    eqDoc (emptyDoc <> x) x

---------------------------------------------------------------------
-- Text homomorphism
---------------------------------------------------------------------

-- P176: text (s ++ t) = text s <> text t
prop_P176_text_homo :: Property
prop_P176_text_homo = property $ do
    s <- forAll $ Gen.string (Range.linear 0 10) Gen.alpha
    t <- forAll $ Gen.string (Range.linear 0 10) Gen.alpha
    eqDoc (fromString (s ++ t)) (fromString s <> fromString t)

-- P177: text "" = empty
prop_P177_text_empty :: Property
prop_P177_text_empty = property $ do
    eqDoc (fromString "" :: Doc) emptyDoc

---------------------------------------------------------------------
-- Nest laws
---------------------------------------------------------------------

-- P178: nest (i + j) x = nest i (nest j x)
prop_P178_nest_add :: Property
prop_P178_nest_add = property $ do
    i <- forAll $ Gen.int (Range.linear 0 8)
    j <- forAll $ Gen.int (Range.linear 0 8)
    x <- forAllDoc genDoc
    eqDoc (nest (i + j) x) (nest i (nest j x))

-- P179: nest 0 x = x
prop_P179_nest_zero :: Property
prop_P179_nest_zero = property $ do
    x <- forAllDoc genDoc
    eqDoc (nest 0 x) x

-- P180: nest i (x <> y) = nest i x <> nest i y
prop_P180_nest_cat :: Property
prop_P180_nest_cat = property $ do
    i <- forAll $ Gen.int (Range.linear 0 8)
    x <- forAllDoc genDoc
    y <- forAllDoc genDoc
    eqDoc (nest i (x <> y)) (nest i x <> nest i y)

-- P181: nest i empty = empty
prop_P181_nest_empty :: Property
prop_P181_nest_empty = property $ do
    i <- forAll $ Gen.int (Range.linear 0 8)
    eqDoc (nest i emptyDoc) emptyDoc

-- P182: nest i (text s) = text s
prop_P182_nest_text :: Property
prop_P182_nest_text = property $ do
    i <- forAll $ Gen.int (Range.linear 0 8)
    s <- forAll $ Gen.string (Range.linear 1 10) Gen.alpha
    eqDoc (nest i (short s)) (short s)

-- P183: nest i (align x) = align x
prop_P183_nest_align :: Property
prop_P183_nest_align = property $ do
    i <- forAll $ Gen.int (Range.linear 0 8)
    x <- forAllDoc genDoc
    eqDoc (nest i (align x)) (align x)

---------------------------------------------------------------------
-- Group laws
---------------------------------------------------------------------

-- P184: group empty = empty
prop_P184_group_empty :: Property
prop_P184_group_empty = property $ do
    eqDoc (group emptyDoc) (emptyDoc :: Doc)

-- P185: group (text s <> x) = text s <> group x
prop_P185_group_text :: Property
prop_P185_group_text = property $ do
    s <- forAll $ Gen.string (Range.linear 1 8) Gen.alpha
    x <- forAllDoc genDoc
    eqDoc (group (short s <> x)) (short s <> group x)

-- P186: group (nest i x) = nest i (group x)
prop_P186_group_nest :: Property
prop_P186_group_nest = property $ do
    i <- forAll $ Gen.int (Range.linear 0 8)
    x <- forAllDoc genDoc
    eqDoc (group (nest i x)) (nest i (group x))

-- P187: group (align x) = align (group x)
prop_P187_group_align :: Property
prop_P187_group_align = property $ do
    x <- forAllDoc genDoc
    eqDoc (group (align x)) (align (group x))

---------------------------------------------------------------------
-- Align laws
---------------------------------------------------------------------

-- P188: align empty = empty
prop_P188_align_empty :: Property
prop_P188_align_empty = property $ do
    eqDoc (align emptyDoc) (emptyDoc :: Doc)

-- P189: align (text s) = text s
prop_P189_align_text :: Property
prop_P189_align_text = property $ do
    s <- forAll $ Gen.string (Range.linear 1 10) Gen.alpha
    eqDoc (align (short s)) (short s)

-- P190: align (align x) = align x
prop_P190_align_idempotent :: Property
prop_P190_align_idempotent = property $ do
    x <- forAllDoc genDoc
    eqDoc (align (align x)) (align x)

---------------------------------------------------------------------
-- Line break operator laws
--
-- x <$> y  =  x <> line <> y  (vsep-style)
---------------------------------------------------------------------

-- | The <$> operator from Wadler's paper.
infixr 5 <$$>
(<$$>) :: Doc -> Doc -> Doc
x <$$> y = x <> line <> y

-- P191: x <$> (y <$> z) = (x <$> y) <$> z
prop_P191_line_assoc :: Property
prop_P191_line_assoc = property $ do
    x <- forAllDoc genDoc
    y <- forAllDoc genDoc
    z <- forAllDoc genDoc
    eqDoc (x <$$> (y <$$> z)) ((x <$$> y) <$$> z)

-- P192: x <> (y <$> z) = (x <> y) <$> z
prop_P192_cat_line_assoc_r :: Property
prop_P192_cat_line_assoc_r = property $ do
    x <- forAllDoc genDoc
    y <- forAllDoc genDoc
    z <- forAllDoc genDoc
    eqDoc (x <> (y <$$> z)) ((x <> y) <$$> z)

-- P193: x <$> (y <> z) = (x <$> y) <> z
prop_P193_line_cat_assoc_r :: Property
prop_P193_line_cat_assoc_r = property $ do
    x <- forAllDoc genDoc
    y <- forAllDoc genDoc
    z <- forAllDoc genDoc
    eqDoc (x <$$> (y <> z)) ((x <$$> y) <> z)
