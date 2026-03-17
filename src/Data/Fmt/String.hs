{-# LANGUAGE DerivingVia #-}

-- | String formatting via 'ShowS'-backed 'Builder'.
--
-- @
-- import Data.Fmt
-- import Data.Fmt.String
--
-- runStringFmt $ "hello" % " " % "world"
-- -- "hello world"
-- @
module Data.Fmt.String (
    -- * Builder
    Builder (..),

    -- * StringFmt
    StringFmt,
    runStringFmt,
) where

import Data.Fmt.Type (Fmt (..))
import Data.Monoid (Endo (..))
import Data.String (IsString (..))

-- | A 'ShowS'-backed string builder with O(1) concatenation.
newtype Builder = Builder {unBuilder :: ShowS}
    deriving (Semigroup, Monoid) via (Endo String)

instance IsString Builder where
    fromString s = Builder (s ++)

instance Show Builder where
    show (Builder f) = f ""

-- | A 'Fmt' specialized to 'Builder'.
type StringFmt = Fmt Builder

-- | Run a 'StringFmt' to produce a 'String'.
runStringFmt :: StringFmt String a -> a
runStringFmt (Fmt f) = f (\(Builder s) -> s "")
