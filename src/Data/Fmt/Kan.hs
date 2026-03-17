{-# LANGUAGE RankNTypes #-}

-- | Kan extension connections for 'Fmt'.
--
-- Re-exports "Data.Functor.Kan" from @scheme-extensions@ and adds
-- 'fmtDay' which is specific to the 'Fmt' type.
module Data.Fmt.Kan (
    -- * Fmt-specific
    fmtDay,

    -- * Re-exports from scheme-extensions
    module Data.Functor.Kan,
) where

import Data.Fmt.Type (Fmt (..))
import Data.Functor.Day (Day (..))
import Data.Functor.Kan

-- | Witness that @(%)@ is Day convolution of @(->) m@.
--
-- Decomposes two formatters into their @(->) m@ components
-- and pairs them in a Day product.
--
-- >>> import Data.Fmt
-- >>> import Data.Fmt.Kan
-- >>> dap (fmtDay (fmt "hello " :: Fmt String String String) (fmt "world")) ""
-- "hello world"
fmtDay
    :: Semigroup m
    => Fmt m m m
    -> Fmt m m m
    -> Day ((->) m) ((->) m) m
fmtDay (Fmt f) (Fmt g) = Day (\m -> f (<> m)) (\m -> g (<> m)) (<>)
