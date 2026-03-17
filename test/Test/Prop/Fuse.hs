{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Prop.Fuse (tests) where

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
-- P201–P207: Fuse
---------------------------------------------------------------------

-- P201: fuse preserves semantics
prop_P201_fuse_semantics :: Property
prop_P201_fuse_semantics = property $ do
    doc <- forAllDoc genDoc
    eqDoc (fuse doc) doc

-- P202: fuse merges adjacent leaves
prop_P202_fuse_leaves :: Property
prop_P202_fuse_leaves = property $ do
    let doc = short "hello" <> short " " <> short "world"
        fused = fuse doc
    pretty (opts 80) fused === "hello world"

-- P203: fuse merges nested Nest
prop_P203_fuse_nest :: Property
prop_P203_fuse_nest = property $ do
    let doc = nest 2 (nest 3 (hardline <> short "x"))
        fused = fuse doc
    pretty (opts 80) fused === pretty (opts 80) doc

-- P204: fuse empty = empty
prop_P204_fuse_empty :: Property
prop_P204_fuse_empty = property $ do
    eqDoc (fuse (emptyDoc :: Doc)) emptyDoc

-- P205: fuse (fuse doc) = fuse doc (idempotent)
prop_P205_fuse_idempotent :: Property
prop_P205_fuse_idempotent = property $ do
    doc <- forAllDoc genDoc
    eqDoc (fuse (fuse doc)) (fuse doc)

-- P206: fuse preserves semantics on complex docs
prop_P206_fuse_complex :: Property
prop_P206_fuse_complex = property $ do
    doc <- forAllDoc genDoc
    w <- forAll $ Gen.element [5, 10, 20, 40, 80]
    pretty (opts w) (fuse doc) === pretty (opts w) doc

-- P207: fuse with streaming layout agrees
prop_P207_fuse_stream :: Property
prop_P207_fuse_stream = property $ do
    doc <- forAllDoc genDoc
    let o = opts 80
    prettyStream o (fuse doc) === pretty o doc
