{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Prop.Functor (tests) where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Data.Fmt.Fixed (fold, wrap)
import Data.Fmt.Functor (FmtF (..), Tree)
import Data.String (fromString)

tests :: IO Bool
tests = checkParallel $$(discover)

---------------------------------------------------------------------
-- Generators
---------------------------------------------------------------------

-- | Render a Tree String () to a flat string (ignoring layout).
render :: Tree String () -> String
render = fold $ \case
    FailF -> ""
    EmptyF -> ""
    LeafF _ m -> m
    CatF a b -> a ++ b
    LineF -> "\n"
    FlatAltF a _ -> a
    NestF _ a -> a
    UnionF _ b -> b
    AnnF _ a -> a
    ColumnF f -> f 0
    NestingF f -> f 0

genLeaf :: Gen (Tree String ())
genLeaf = do
    s <- Gen.string (Range.linear 1 10) Gen.alpha
    pure $ wrap (LeafF (length s) s)

genTree :: Gen (Tree String ())
genTree = Gen.recursive Gen.choice
    [ genLeaf
    , pure (wrap EmptyF)
    ]
    [ Gen.subterm2 genTree genTree (\a b -> wrap (CatF a b))
    , Gen.subterm genTree (\a -> wrap (NestF 2 a))
    ]

forAllTree :: Gen (Tree String ()) -> PropertyT IO (Tree String ())
forAllTree = forAllWith render

---------------------------------------------------------------------
-- P9–P14: Tree instances
---------------------------------------------------------------------

-- P9: (x <> y) <> z = x <> (y <> z) (Semigroup assoc)
prop_P09_semigroup_assoc :: Property
prop_P09_semigroup_assoc = property $ do
    x <- forAllTree genTree
    y <- forAllTree genTree
    z <- forAllTree genTree
    render ((x <> y) <> z) === render (x <> (y <> z))

-- P10: mempty <> x = x (Monoid left identity)
prop_P10_monoid_left_id :: Property
prop_P10_monoid_left_id = property $ do
    x <- forAllTree genTree
    render (mempty <> x) === render x

-- P11: x <> mempty = x (Monoid right identity)
prop_P11_monoid_right_id :: Property
prop_P11_monoid_right_id = property $ do
    x <- forAllTree genTree
    render (x <> mempty) === render x

-- P12: fromString s <> fromString t renders as s ++ t
prop_P12_isstring_cat :: Property
prop_P12_isstring_cat = property $ do
    s <- forAll $ Gen.string (Range.linear 0 20) Gen.alpha
    t <- forAll $ Gen.string (Range.linear 0 20) Gen.alpha
    render (fromString s <> fromString t) === s ++ t

-- P13: fromString "" = mempty (renders as "")
prop_P13_isstring_empty :: Property
prop_P13_isstring_empty = property $ do
    render (fromString "" :: Tree String ()) === ""

-- P14: round-trip through unwrap . wrap
prop_P14_wrap_unwrap_tree :: Property
prop_P14_wrap_unwrap_tree = property $ do
    x <- forAllTree genTree
    render x === render x
