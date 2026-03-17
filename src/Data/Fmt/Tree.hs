{-# LANGUAGE LambdaCase #-}

-- | Pretty-printer API over 'FmtF' document trees.
--
-- Smart constructors, combinators, layout algorithms, and rendering.
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

    -- * Tokens
    Token (..),

    -- * Layout
    PageWidth (..),
    LayoutOptions (..),
    defaultLayoutOptions,
    layoutPretty,
    layoutCompact,

    -- * Rendering
    render,
    pretty,
) where

import Data.Fmt.Fixed (Fix, fold, unwrap, wrap)
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

---------------------------------------------------------------------
-- Tokens
---------------------------------------------------------------------

-- | A single rendered token. The output of the layout algorithm.
data Token m ann
    = TLeaf !Int !m     -- ^ Content with display width
    | TLine !Int        -- ^ Newline followed by @n@ indentation spaces
    | TAnnPush ann      -- ^ Begin annotation scope
    | TAnnPop           -- ^ End annotation scope
    deriving (Show, Eq)

---------------------------------------------------------------------
-- Layout
---------------------------------------------------------------------

-- | Page width configuration.
data PageWidth
    = AvailablePerLine !Int !Double
    -- ^ @AvailablePerLine maxColumns ribbonFraction@
    | Unbounded
    deriving (Show, Eq)

-- | Layout options.
newtype LayoutOptions = LayoutOptions
    { layoutPageWidth :: PageWidth
    }
    deriving (Show, Eq)

-- | 80 columns, ribbon fraction 1.0.
defaultLayoutOptions :: LayoutOptions
defaultLayoutOptions = LayoutOptions (AvailablePerLine 80 1.0)

-- | Commands for the layout pipeline.
data Cmd m ann
    = CDoc !Int (Tree m ann)   -- ^ Process document at nesting level
    | CPopAnn                  -- ^ Emit TAnnPop

-- | Wadler/Leijen layout with one-line lookahead.
--
-- At 'Union' nodes, tries the flattened (first) branch. If the
-- remainder of the line fits within the page width, uses it.
-- Otherwise falls back to the second (default) branch.
layoutPretty :: LayoutOptions -> Tree m ann -> [Token m ann]
layoutPretty opts doc = case best 0 [CDoc 0 doc] of
    Nothing -> []  -- should not happen for well-formed docs
    Just tokens -> tokens
  where
    pageWidth = case layoutPageWidth opts of
        AvailablePerLine w _ -> w
        Unbounded -> maxBound

    best :: Int -> [Cmd m ann] -> Maybe [Token m ann]
    best _ [] = Just []
    best cc (CPopAnn : rest) = (TAnnPop :) <$> best cc rest
    best cc (CDoc i d : rest) = case unwrap d of
        Fail -> Nothing  -- layout failure
        Empty -> best cc rest
        Leaf len m -> (TLeaf len m :) <$> best (cc + len) rest
        Cat x y -> best cc (CDoc i x : CDoc i y : rest)
        Line -> (TLine i :) <$> best i rest
        FlatAlt x _ -> best cc (CDoc i x : rest)
        Nest j x -> best cc (CDoc (i + j) x : rest)
        Union x y ->
            case best cc (CDoc i x : rest) of
                Just flatTokens | fits (pageWidth - cc) flatTokens -> Just flatTokens
                _ -> best cc (CDoc i y : rest)
        Ann a x -> (TAnnPush a :) <$> best cc (CDoc i x : CPopAnn : rest)
        Column f -> best cc (CDoc i (f cc) : rest)
        Nesting f -> best cc (CDoc i (f i) : rest)

-- | One-line lookahead: does the content fit in @w@ characters
-- before the next line break?
fits :: Int -> [Token m ann] -> Bool
fits w _ | w < 0 = False
fits _ [] = True
fits _ (TLine _ : _) = True
fits w (TLeaf len _ : rest) = fits (w - len) rest
fits w (TAnnPush _ : rest) = fits w rest
fits w (TAnnPop : rest) = fits w rest

-- | Compact layout: no width-sensitivity, always breaks.
--
-- Every 'FlatAlt' uses its default, every 'Union' uses the
-- narrow branch. 'Column' and 'Nesting' receive 0.
layoutCompact :: Tree m ann -> [Token m ann]
layoutCompact doc = go 0 [doc]
  where
    go _ [] = []
    go cc (d : rest) = case unwrap d of
        Fail -> []
        Empty -> go cc rest
        Leaf len m -> TLeaf len m : go (cc + len) rest
        Cat x y -> go cc (x : y : rest)
        Line -> TLine 0 : go 0 rest
        FlatAlt x _ -> go cc (x : rest)
        Nest _ x -> go cc (x : rest)  -- ignore nesting in compact
        Union _ y -> go cc (y : rest)  -- always narrow
        Ann a x -> TAnnPush a : go cc (x : rest) -- no pop tracking in compact
        Column f -> go cc (f cc : rest)
        Nesting f -> go cc (f 0 : rest)

---------------------------------------------------------------------
-- Rendering
---------------------------------------------------------------------

-- | Render a token stream to the output monoid, discarding annotations.
render :: (Monoid m, IsString m) => [Token m ann] -> m
render = go
  where
    go [] = mempty
    go (TLeaf _ m : rest) = m <> go rest
    go (TLine i : rest) = fromString ('\n' : replicate i ' ') <> go rest
    go (TAnnPush _ : rest) = go rest
    go (TAnnPop : rest) = go rest

-- | Lay out and render a document.
--
-- @pretty opts = render . layoutPretty opts@
pretty :: (Monoid m, IsString m) => LayoutOptions -> Tree m ann -> m
pretty opts = render . layoutPretty opts
