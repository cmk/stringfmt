{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Pattern functor for document trees.
--
-- @FmtF m ann@ is the base functor for pretty-printer documents,
-- parametric over content type @m@ and annotation type @ann@.
--
-- Changes from prettyprinter's @DocF@:
--
-- * @Char@/@Text@ merged into @LeafF m@ (parametric content)
-- * @WithPageWidth@ dropped (recoverable via @ColumnF@/@NestingF@)
-- * @FailF@ retained for lazy failure propagation through @ColumnF@/@NestingF@
module Data.Fmt.Functor (
    -- * Pattern functor
    FmtF (..),

    -- * Tree
    Tree,
) where

import Data.Fmt.Fixed (Fix, wrap)
import Data.String (IsString (..))

-- | One layer of a document tree.
--
-- @r@ marks recursive positions. @m@ is the content type
-- (e.g. 'Data.Text.Text', 'Data.ByteString.Builder.Builder').
-- @ann@ is the annotation type (e.g. ANSI styles, HTML tags).
data FmtF m ann r
    = -- | Layout failure. Produced by 'Data.Fmt.Tree.flatten' on
      -- hard line breaks; consumed by the layout algorithm to
      -- reject a flattened branch.
      FailF
    | -- | Empty document.
      EmptyF
    | -- | Literal content with cached display width.
      LeafF !Int !m
    | -- | Concatenation.
      CatF r r
    | -- | Hard line break. Cannot be flattened (becomes 'FailF').
      LineF
    | -- | @FlatAltF default flat@: use @default@ normally,
      -- @flat@ when flattened by 'Data.Fmt.Tree.group'.
      FlatAltF r r
    | -- | @NestF i doc@: increase nesting by @i@ for @doc@.
      NestF !Int r
    | -- | @UnionF wide narrow@: layout alternatives.
      -- Invariant: @wide@ is the flattened form of @narrow@.
      -- Internal — constructed only by 'Data.Fmt.Tree.group'.
      UnionF r r
    | -- | Annotated document.
      AnnF ann r
    | -- | React to the current column position.
      ColumnF (Int -> r)
    | -- | React to the current nesting level.
      NestingF (Int -> r)
    deriving (Functor)

-- | A document tree: the initial algebra of @FmtF m ann@.
type Tree m ann = Fix (FmtF m ann)

---------------------------------------------------------------------
-- Instances
---------------------------------------------------------------------

instance Semigroup (Tree m ann) where
    {-# INLINE (<>) #-}
    x <> y = wrap (CatF x y)

instance Monoid (Tree m ann) where
    {-# INLINE mempty #-}
    mempty = wrap EmptyF

instance IsString m => IsString (Tree m ann) where
    {-# INLINE fromString #-}
    fromString s = wrap (LeafF (length s) (fromString s))
