{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Prop.Tree (tests) where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Data.Fmt.Fixed (fold)
import Data.Fmt.Functor (FmtF (..), Tree)
import Data.Fmt.Tree

tests :: IO Bool
tests = checkParallel $$(discover)

---------------------------------------------------------------------
-- Generators
---------------------------------------------------------------------

-- | Simple render: flatten tree to string ignoring layout.
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

-- | Render the flattened (single-line) version.
renderFlat :: Tree String () -> Maybe String
renderFlat = fold $ \case
    FailF -> Nothing
    EmptyF -> Just ""
    LeafF _ m -> Just m
    CatF a b -> (++) <$> a <*> b
    LineF -> Nothing
    FlatAltF _ b -> b
    NestF _ a -> a
    UnionF a _ -> a
    AnnF _ a -> a
    ColumnF f -> f 0
    NestingF f -> f 0

genLeaf :: Gen (Tree String ())
genLeaf = do
    s <- Gen.string (Range.linear 1 10) Gen.alpha
    pure $ leaf (length s) s

forAllTree :: Gen (Tree String ()) -> PropertyT IO (Tree String ())
forAllTree = forAllWith render

---------------------------------------------------------------------
-- P15–P20: Tree combinators
---------------------------------------------------------------------

-- P15: flatten hardline = fail_
prop_P15_flatten_hardline :: Property
prop_P15_flatten_hardline = property $ do
    renderFlat (flatten hardline) === Nothing

-- P16: flatten (flatAlt x y) = flatten y
prop_P16_flatten_flatalt :: Property
prop_P16_flatten_flatalt = property $ do
    x <- forAllTree genLeaf
    y <- forAllTree genLeaf
    renderFlat (flatten (flatAlt x y)) === renderFlat (flatten y)

-- P17: flatten (union x y) = flatten x
prop_P17_flatten_union :: Property
prop_P17_flatten_union = property $ do
    x <- forAllTree genLeaf
    y <- forAllTree genLeaf
    renderFlat (flatten (union x y)) === renderFlat (flatten x)

-- P18: flatten (leaf n m) = leaf n m
prop_P18_flatten_leaf :: Property
prop_P18_flatten_leaf = property $ do
    s <- forAll $ Gen.string (Range.linear 1 10) Gen.alpha
    renderFlat (flatten (leaf (length s) s)) === Just s

-- P19: group x = union (flatten x) x
prop_P19_group_def :: Property
prop_P19_group_def = property $ do
    x <- forAllTree genLeaf
    render (group x) === render (union (flatten x) x)

-- P20: align at default position is identity
prop_P20_align :: Property
prop_P20_align = property $ do
    x <- forAllTree genLeaf
    render (align x) === render x
