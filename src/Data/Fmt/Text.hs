{-# LANGUAGE OverloadedStrings #-}

-- | Text formatting via 'Data.Text.Lazy.Builder'.
module Data.Fmt.Text (
    -- * TextFmt
    TextFmt,
    runTextFmt,
    printf,

    -- * Combinators
    cat1With,
    hsep,
    vsep,
    hang,
    list1,

    -- * Splitting
    replace1,
    splitWith,

    -- * Running Fmt1
    run1,

    -- * Structured output
    jsonList,
    yamlList,
    jsonMap,
    yamlMap,
) where

import Data.Foldable (toList)
import Data.Fmt.Type (Fmt (..), Fmt1, fmt, fmt1, (%))

import qualified Data.Text.IO as TIO
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (Builder, fromText, toLazyText)

-- | A 'Fmt' specialized to 'Data.Text.Lazy.Builder.Builder'.
type TextFmt = Fmt Builder

-- | Run a 'TextFmt' to produce lazy 'TL.Text'.
{-# INLINE runTextFmt #-}
runTextFmt :: TextFmt TL.Text a -> a
runTextFmt (Fmt f) = f toLazyText

-- | Run a 'TextFmt' and print the result to stdout.
{-# INLINE printf #-}
printf :: TextFmt (IO ()) a -> a
printf (Fmt f) = f $ TIO.putStr . TL.toStrict . toLazyText

---------------------------------------------------------------------
-- Internal
---------------------------------------------------------------------

{-# INLINE toStrict #-}
toStrict :: Builder -> Text
toStrict = TL.toStrict . toLazyText

-- | Run a Fmt1 to strict Text.
{-# INLINE run1 #-}
run1 :: Fmt1 Builder Text a -> a -> Text
run1 (Fmt f) = f (TL.toStrict . toLazyText)

---------------------------------------------------------------------
-- Combinators
---------------------------------------------------------------------

-- | Format each value in a foldable and join the results.
{-# INLINEABLE cat1With #-}
cat1With ::
    Foldable f =>
    ([Text] -> Text) ->
    Fmt1 Builder Text a ->
    Fmt1 Builder s (f a)
cat1With join f = fmt1 $ fromText . join . map (run1 f) . toList

-- | Format each value with spaces in between.
{-# INLINE hsep #-}
hsep :: Foldable f => Fmt1 Builder Text a -> Fmt1 Builder s (f a)
hsep = cat1With (T.intercalate " ")

-- | Format each value on its own line.
{-# INLINE vsep #-}
vsep :: Foldable f => Fmt1 Builder Text a -> Fmt1 Builder s (f a)
vsep = cat1With (T.intercalate "\n")

-- | Format each value on its own line, indented by @n@ spaces.
{-# INLINE hang #-}
hang :: Foldable f => Int -> Fmt1 Builder Text a -> Fmt1 Builder s (f a)
hang n f = fmt1 $ \xs ->
    let pad = T.replicate n " "
        items = map (\x -> pad <> run1 f x) (toList xs)
     in fromText (T.unlines items)

-- | Format in square brackets with comma separation.
{-# INLINE list1 #-}
list1 :: Foldable f => Fmt1 Builder Text a -> Fmt1 Builder s (f a)
list1 = cat1With (\xs -> "[" <> T.intercalate ", " xs <> "]")

---------------------------------------------------------------------
-- Splitting
---------------------------------------------------------------------

-- | Split the formatted output using a splitting function, then
-- rejoin with a custom combinator.
{-# INLINE splitWith #-}
splitWith ::
    (Text -> (Text, Text)) ->
    (Text -> Text -> Fmt Builder a2 a1) ->
    Fmt Builder a1 b ->
    Fmt Builder a2 b
splitWith brk join f = f `bind_` \b ->
    let (l, r) = brk (toStrict b)
     in uncurry join (l, r)
  where
    bind_ m g = Fmt $ \k -> unFmt m (\a -> unFmt (g a) k)

-- | Replace the first occurrence of a search term.
{-# INLINE replace1 #-}
replace1 :: Text -> Fmt Builder a a -> Fmt Builder a b -> Fmt Builder a b
replace1 needle replacement =
    splitWith (T.breakOn needle) $ \l r0 ->
        case T.stripPrefix needle r0 of
            Nothing -> fmt (fromText l)
            Just r -> fmt (fromText l) % replacement % fmt (fromText r)

---------------------------------------------------------------------
-- Structured output
---------------------------------------------------------------------

-- | Format a foldable as a JSON-style list.
{-# INLINE jsonList #-}
jsonList :: Foldable f => Fmt1 Builder Text a -> Fmt1 Builder s (f a)
jsonList = cat1With $ \xs -> "[" <> T.intercalate ", " xs <> "]"

-- | Format a foldable as a YAML-style list.
{-# INLINE yamlList #-}
yamlList :: Foldable f => Fmt1 Builder Text a -> Fmt1 Builder s (f a)
yamlList = cat1With $ \xs -> T.unlines (map ("- " <>) xs)

-- | Format key-value pairs as a JSON-style map.
{-# INLINE jsonMap #-}
jsonMap :: (Foldable f) => Fmt1 Builder Text k -> Fmt1 Builder Text v -> Fmt1 Builder s (f (k, v))
jsonMap kf vf = fmt1 $ \kvs ->
    let items = map fmtPair (toList kvs)
        fmtPair (k, v) = run1 kf k <> ": " <> run1 vf v
     in fromText $ "{" <> T.intercalate ", " items <> "}"

-- | Format key-value pairs as a YAML-style map.
{-# INLINE yamlMap #-}
yamlMap :: (Foldable f) => Fmt1 Builder Text k -> Fmt1 Builder Text v -> Fmt1 Builder s (f (k, v))
yamlMap kf vf = fmt1 $ \kvs ->
    let items = map fmtPair (toList kvs)
        fmtPair (k, v) = run1 kf k <> ": " <> run1 vf v
     in fromText $ T.unlines items
