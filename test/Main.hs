module Main (main) where

import System.Exit (exitFailure, exitSuccess)

import qualified Test.Prop.Fixed as Fixed
import qualified Test.Prop.Fuse as Fuse
import qualified Test.Prop.Group as Group
import qualified Test.Prop.Functor as Functor
import qualified Test.Prop.Tree as Tree
import qualified Test.Prop.Fmt as Fmt
import qualified Test.Prop.String as String
import qualified Test.Prop.ByteString as ByteString
import qualified Test.Prop.Combinators as Combinators
import qualified Test.Prop.Cons as Cons
import qualified Test.Prop.Code as Code
import qualified Test.Prop.Kan as Kan
import qualified Test.Prop.Layout as Layout
import qualified Test.Prop.Stream as Stream
import qualified Test.Prop.Text as Text
import qualified Test.Prop.Wadler as Wadler

main :: IO ()
main = do
    ok <- and <$> sequence
        [ Fixed.tests
        , Fuse.tests
        , Group.tests
        , Functor.tests
        , Tree.tests
        , Fmt.tests
        , String.tests
        , ByteString.tests
        , Combinators.tests
        , Cons.tests
        , Code.tests
        , Kan.tests
        , Layout.tests
        , Stream.tests
        , Text.tests
        , Wadler.tests
        ]
    if ok then exitSuccess else exitFailure
