{-# LANGUAGE OverloadedStrings #-}

-- | ByteString formatting via 'Data.ByteString.Builder.Builder'.
module Data.Fmt.ByteString (
    -- * ByteFmt
    ByteFmt,
    runByteFmt,
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
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder, byteString, toLazyByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL

-- | A 'Fmt' specialized to 'Builder'.
type ByteFmt = Fmt Builder

-- | Run a 'ByteFmt' to produce a lazy 'BL.ByteString'.
{-# INLINE runByteFmt #-}
runByteFmt :: ByteFmt BL.ByteString a -> a
runByteFmt (Fmt f) = f toLazyByteString

-- | Run a 'ByteFmt' and print the result to stdout.
{-# INLINE printf #-}
printf :: ByteFmt (IO ()) a -> a
printf (Fmt f) = f $ BL.putStr . toLazyByteString

---------------------------------------------------------------------
-- Internal
---------------------------------------------------------------------

-- | Convert a Builder to strict ByteString (for splitting operations).
{-# INLINE toStrict #-}
toStrict :: Builder -> ByteString
toStrict = BL.toStrict . toLazyByteString

-- | Run a Fmt1 to strict ByteString.
{-# INLINE run1 #-}
run1 :: Fmt1 Builder ByteString a -> a -> ByteString
run1 (Fmt f) = f (BL.toStrict . toLazyByteString)

---------------------------------------------------------------------
-- Combinators
---------------------------------------------------------------------

-- | Format each value in a foldable and join the results.
{-# INLINEABLE cat1With #-}
cat1With ::
    Foldable f =>
    ([ByteString] -> ByteString) ->
    Fmt1 Builder ByteString a ->
    Fmt1 Builder s (f a)
cat1With join f = fmt1 $ byteString . join . map (run1 f) . toList

-- | Format each value with spaces in between.
--
-- >>> runByteFmt (hsep (fmt1 byteString)) ["one", "two", "three"]
-- "one two three"
{-# INLINE hsep #-}
hsep :: Foldable f => Fmt1 Builder ByteString a -> Fmt1 Builder s (f a)
hsep = cat1With (B.intercalate " ")

-- | Format each value on its own line.
{-# INLINE vsep #-}
vsep :: Foldable f => Fmt1 Builder ByteString a -> Fmt1 Builder s (f a)
vsep = cat1With B.unlines

-- | Format each value on its own line, indented by @n@ spaces.
{-# INLINE hang #-}
hang :: Foldable f => Int -> Fmt1 Builder ByteString a -> Fmt1 Builder s (f a)
hang n f = fmt1 $ \xs ->
    let pad = B.replicate n ' '
        items = map (\x -> pad <> run1 f x) (toList xs)
     in byteString (B.unlines items)

-- | Format in square brackets with comma separation.
--
-- >>> runByteFmt (list1 (fmt1 byteString)) ["one", "two"]
-- "[one, two]"
{-# INLINE list1 #-}
list1 :: Foldable f => Fmt1 Builder ByteString a -> Fmt1 Builder s (f a)
list1 = cat1With (\xs -> "[" <> B.intercalate ", " xs <> "]")

---------------------------------------------------------------------
-- Splitting
---------------------------------------------------------------------

-- | Split the formatted output using a splitting function, then
-- rejoin with a custom combinator.
{-# INLINE splitWith #-}
splitWith ::
    (ByteString -> (ByteString, ByteString)) ->
    (ByteString -> ByteString -> Fmt Builder a2 a1) ->
    Fmt Builder a1 b ->
    Fmt Builder a2 b
splitWith brk join f = f `bind_` \b ->
    let (l, r) = brk (toStrict b)
     in uncurry join (l, r)
  where
    bind_ m g = Fmt $ \k -> unFmt m (\a -> unFmt (g a) k)

-- | Replace the first occurrence of a search term.
--
-- >>> runByteFmt (replace1 "bar" "FOO" (fmt (byteString "foobarbaz")))
-- "fooFOObaz"
{-# INLINE replace1 #-}
replace1 :: ByteString -> Fmt Builder a a -> Fmt Builder a b -> Fmt Builder a b
replace1 needle replacement =
    splitWith (B.breakSubstring needle) $ \l r0 ->
        case B.stripPrefix needle r0 of
            Nothing -> fmt (byteString l)
            Just r -> fmt (byteString l) % replacement % fmt (byteString r)

---------------------------------------------------------------------
-- Structured output
---------------------------------------------------------------------

-- | Format a foldable as a JSON-style list.
--
-- >>> printf (jsonList (fmt1 byteString)) ["one", "two"]
-- ["one", "two"]
{-# INLINE jsonList #-}
jsonList :: Foldable f => Fmt1 Builder ByteString a -> Fmt1 Builder s (f a)
jsonList = cat1With $ \xs -> "[" <> B.intercalate ", " xs <> "]"

-- | Format a foldable as a YAML-style list.
--
-- >>> printf (yamlList (fmt1 byteString)) ["one", "two"]
-- - one
-- - two
{-# INLINE yamlList #-}
yamlList :: Foldable f => Fmt1 Builder ByteString a -> Fmt1 Builder s (f a)
yamlList = cat1With $ \xs -> B.unlines (map ("- " <>) xs)

-- | Format key-value pairs as a JSON-style map.
{-# INLINE jsonMap #-}
jsonMap :: (Foldable f) => Fmt1 Builder ByteString k -> Fmt1 Builder ByteString v -> Fmt1 Builder s (f (k, v))
jsonMap kf vf = fmt1 $ \kvs ->
    let items = map fmtPair (toList kvs)
        fmtPair (k, v) = run1 kf k <> ": " <> run1 vf v
     in byteString $ "{" <> B.intercalate ", " items <> "}"

-- | Format key-value pairs as a YAML-style map.
{-# INLINE yamlMap #-}
yamlMap :: (Foldable f) => Fmt1 Builder ByteString k -> Fmt1 Builder ByteString v -> Fmt1 Builder s (f (k, v))
yamlMap kf vf = fmt1 $ \kvs ->
    let items = map fmtPair (toList kvs)
        fmtPair (k, v) = run1 kf k <> ": " <> run1 vf v
     in byteString $ B.unlines items
