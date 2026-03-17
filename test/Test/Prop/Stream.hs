{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Prop.Stream (tests) where

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

---------------------------------------------------------------------
-- P194–P200: Streaming layout
---------------------------------------------------------------------

-- P194: prettyStream agrees with pretty
prop_P194_stream_agrees :: Property
prop_P194_stream_agrees = property $ do
    doc <- forAllDoc genDoc
    w <- forAll $ Gen.element [5, 10, 20, 40, 80]
    let o = opts w
    prettyStream o doc === pretty o doc

-- P195: prettyStream with Unbounded agrees
prop_P195_stream_unbounded :: Property
prop_P195_stream_unbounded = property $ do
    doc <- forAllDoc genDoc
    let o = LayoutOptions Unbounded
    prettyStream o doc === pretty o doc

-- P196: renderStream . layoutStream = render . layoutPretty
prop_P196_render_stream :: Property
prop_P196_render_stream = property $ do
    doc <- forAllDoc genDoc
    let o = opts 80
    renderStream (layoutStream o doc) === render (layoutPretty o doc)

-- P197: streaming handles group width-sensitivity
prop_P197_stream_group :: Property
prop_P197_stream_group = property $ do
    let doc = group (short "hello" <> line <> short "world")
    prettyStream (opts 80) doc === "hello world"
    prettyStream (opts 5) doc === "hello\nworld"

-- P198: streaming handles align
prop_P198_stream_align :: Property
prop_P198_stream_align = property $ do
    let doc = short "key: " <> align (short "line1" <> hardline <> short "line2")
    prettyStream (opts 80) doc === "key: line1\n     line2"

-- P199: streaming handles nested group
prop_P199_stream_nested :: Property
prop_P199_stream_nested = property $ do
    let inner = group (short "a" <> line <> short "b")
        outer = group (short "(" <> inner <> short ")")
    prettyStream (opts 80) outer === "(a b)"
    prettyStream (opts 3) outer === "(a\nb)"

-- P200: streaming handles annotations
prop_P200_stream_ann :: Property
prop_P200_stream_ann = property $ do
    let doc = T.annotate () (short "hello")
    prettyStream (opts 80) doc === "hello"
