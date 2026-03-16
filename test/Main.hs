module Main (main) where

import System.Exit (exitFailure, exitSuccess)

import qualified Test.Prop.Fixed as Fixed
import qualified Test.Prop.Functor as Functor
import qualified Test.Prop.Tree as Tree
import qualified Test.Prop.Fmt as Fmt
import qualified Test.Prop.String as String

main :: IO ()
main = do
    ok <- and <$> sequence
        [ Fixed.tests
        , Functor.tests
        , Tree.tests
        , Fmt.tests
        , String.tests
        ]
    if ok then exitSuccess else exitFailure
