{-# LANGUAGE LambdaCase #-}

-- | Pretty-printer API over 'FmtF' document trees.
--
-- Smart constructors, combinators, and derived operations
-- expressed as recursion schemes over 'Tree'.
module Data.Fmt.Tree (
    -- * Smart constructors
    fail_,
    emptyDoc,
    leaf,
    hardline,
    line,
    line',
    flatAlt,
    nest,
    union,
    annotate,
    column,
    nesting,

    -- * Combinators
    flatten,
    group,
    align,
) where

import Data.Fmt.Fixed (fold, wrap)
import Data.Fmt.Functor (FmtF (..), Tree)
import Data.String (IsString (..))

---------------------------------------------------------------------
-- Smart constructors
---------------------------------------------------------------------

-- | Layout failure.
{-# INLINE fail_ #-}
fail_ :: Tree m ann
fail_ = wrap Fail

-- | Empty document.
{-# INLINE emptyDoc #-}
emptyDoc :: Tree m ann
emptyDoc = wrap Empty

-- | Literal content with display width.
{-# INLINE leaf #-}
leaf :: Int -> m -> Tree m ann
leaf n m = wrap (Leaf n m)

-- | Hard line break. Cannot be flattened.
{-# INLINE hardline #-}
hardline :: Tree m ann
hardline = wrap Line

-- | Line break, or space when flattened.
{-# INLINE line #-}
line :: IsString m => Tree m ann
line = flatAlt hardline (leaf 1 (fromString " "))

-- | Line break, or empty when flattened.
{-# INLINE line' #-}
line' :: Tree m ann
line' = flatAlt hardline emptyDoc

-- | @flatAlt default flat@: use @default@ normally,
-- @flat@ when flattened by 'group'.
{-# INLINE flatAlt #-}
flatAlt :: Tree m ann -> Tree m ann -> Tree m ann
flatAlt x y = wrap (FlatAlt x y)

-- | Increase nesting by @i@.
{-# INLINE nest #-}
nest :: Int -> Tree m ann -> Tree m ann
nest i = wrap . Nest i

-- | Layout alternatives. Invariant: first argument is the
-- flattened form of the second.
{-# INLINE union #-}
union :: Tree m ann -> Tree m ann -> Tree m ann
union x y = wrap (Union x y)

-- | Attach an annotation.
{-# INLINE annotate #-}
annotate :: ann -> Tree m ann -> Tree m ann
annotate a = wrap . Ann a

-- | React to the current column position.
{-# INLINE column #-}
column :: (Int -> Tree m ann) -> Tree m ann
column = wrap . Column

-- | React to the current nesting level.
{-# INLINE nesting #-}
nesting :: (Int -> Tree m ann) -> Tree m ann
nesting = wrap . Nesting

---------------------------------------------------------------------
-- Combinators
---------------------------------------------------------------------

-- | Replace 'FlatAlt' with its flat branch, 'Line' with 'Fail'.
--
-- Implemented as a fold — children are flattened first,
-- so 'Column'/'Nesting' functions automatically produce
-- flattened subtrees.
flatten :: Tree m ann -> Tree m ann
flatten = fold $ \case
    FlatAlt _ y -> y     -- use the flat alternative
    Line -> fail_        -- hardline can't be flattened
    Union a _ -> a       -- already the flatter branch
    other -> wrap other  -- re-wrap with flattened children

-- | Try to lay out the document on a single line.
-- Falls back to the original if flattening fails.
--
-- @group x = union (flatten x) x@
group :: Tree m ann -> Tree m ann
group x = union (flatten x) x

-- | Lay out relative to the current column rather than
-- the current nesting level.
--
-- @align d = column (\\k -> nesting (\\i -> nest (k - i) d))@
align :: Tree m ann -> Tree m ann
align d = column $ \k -> nesting $ \i -> nest (k - i) d
