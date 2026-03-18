{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Pattern functor for document trees.
--
-- @Doc m ann@ is the base functor for pretty-printer documents,
-- parametric over content type @m@ and annotation type @ann@.
--
-- Changes from prettyprinter's @DocF@:
--
-- * @Char@/@Text@ merged into @Leaf m@ (parametric content)
-- * @WithPageWidth@ dropped (recoverable via @Column@/@Nesting@)
-- * @Fail@ retained for lazy failure propagation through @Column@/@Nesting@
module Data.Fmt.Functor (
    -- * Pattern functor
    Doc (..),

    -- * Tree
    Tree,
) where

import Data.Fmt.Fixed (Mu, wrap)
import Data.String (IsString (..))

-- | One layer of a document tree.
--
-- @r@ marks recursive positions. @m@ is the content type
-- (e.g. 'Data.Text.Text', 'Data.ByteString.Builder.Builder').
-- @ann@ is the annotation type (e.g. ANSI styles, HTML tags).
data Doc m ann r
    = -- | Layout failure. Produced by 'Data.Fmt.Tree.flatten' on
      -- hard line breaks; consumed by the layout algorithm to
      -- reject a flattened branch.
      Fail
    | -- | Empty document.
      Empty
    | -- | Literal content with cached display width.
      Leaf !Int !m
    | -- | Concatenation.
      Cat r r
    | -- | Hard line break. Cannot be flattened (becomes 'Fail').
      Line
    | -- | @FlatAlt default flat@: use @default@ normally,
      -- @flat@ when flattened by 'Data.Fmt.Tree.group'.
      FlatAlt r r
    | -- | @Nest i doc@: increase nesting by @i@ for @doc@.
      Nest !Int r
    | -- | @Union wide narrow@: layout alternatives.
      -- Invariant: @wide@ is the flattened form of @narrow@.
      -- Internal — constructed only by 'Data.Fmt.Tree.group'.
      Union r r
    | -- | Annotated document.
      Ann ann r
    | -- | React to the current column position.
      Column (Int -> r)
    | -- | React to the current nesting level.
      Nesting (Int -> r)
    deriving (Functor)

-- | A document tree: the initial algebra of @Doc m ann@.
type Tree m ann = Mu (Doc m ann)

---------------------------------------------------------------------
-- Instances
---------------------------------------------------------------------

instance Semigroup (Tree m ann) where
    {-# INLINE (<>) #-}
    x <> y = wrap (Cat x y)

instance Monoid (Tree m ann) where
    {-# INLINE mempty #-}
    mempty = wrap Empty

instance IsString m => IsString (Tree m ann) where
    {-# INLINE fromString #-}
    fromString s = wrap (Leaf (length s) (fromString s))
