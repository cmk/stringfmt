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
import Data.List (intercalate, isPrefixOf)
import Data.Fmt.Type (Fmt (..), Fmt1, fmt, fmt1, (%))
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
{-# INLINE runStringFmt #-}
runStringFmt :: StringFmt String a -> a
runStringFmt (Fmt f) = f (\(Builder s) -> s "")

-- | Run a 'StringFmt' and print the result to stdout.
{-# INLINE printf #-}
printf :: StringFmt (IO ()) a -> a
printf (Fmt f) = f $ \(Builder s) -> putStr (s "")

---------------------------------------------------------------------
-- Internal
---------------------------------------------------------------------

-- | Run a Fmt1 to String.
{-# INLINE run1 #-}
run1 :: Fmt1 Builder String a -> a -> String
run1 (Fmt f) = f (\(Builder s) -> s "")

---------------------------------------------------------------------
-- Combinators
---------------------------------------------------------------------

-- | Format each value in a foldable and join the results.
{-# INLINEABLE cat1With #-}
cat1With ::
    Foldable f =>
    ([String] -> String) ->
    Fmt1 Builder String a ->
    Fmt1 Builder s (f a)
cat1With join f = fmt1 $ Builder . (++) . join . map (run1 f) . toList

-- | Format each value with spaces in between.
{-# INLINE hsep #-}
hsep :: Foldable f => Fmt1 Builder String a -> Fmt1 Builder s (f a)
hsep = cat1With (intercalate " ")

-- | Format each value on its own line.
{-# INLINE vsep #-}
vsep :: Foldable f => Fmt1 Builder String a -> Fmt1 Builder s (f a)
vsep = cat1With unlines

-- | Format each value on its own line, indented by @n@ spaces.
{-# INLINE hang #-}
hang :: Foldable f => Int -> Fmt1 Builder String a -> Fmt1 Builder s (f a)
hang n f = fmt1 $ \xs ->
    let pad = replicate n ' '
        items = map (\x -> pad ++ run1 f x) (toList xs)
     in Builder . (++) $ unlines items

-- | Format in square brackets with comma separation.
{-# INLINE list1 #-}
list1 :: Foldable f => Fmt1 Builder String a -> Fmt1 Builder s (f a)
list1 = cat1With (\xs -> "[" ++ intercalate ", " xs ++ "]")

---------------------------------------------------------------------
-- Splitting
---------------------------------------------------------------------

-- | Split the formatted output using a splitting function, then
-- rejoin with a custom combinator.
{-# INLINE splitWith #-}
splitWith ::
    (String -> (String, String)) ->
    (String -> String -> Fmt Builder a2 a1) ->
    Fmt Builder a1 b ->
    Fmt Builder a2 b
splitWith brk join f = f `bind_` \b ->
    let s = show b
        (l, r) = brk s
     in uncurry join (l, r)
  where
    bind_ m g = Fmt $ \k -> unFmt m (\a -> unFmt (g a) k)

-- | Replace the first occurrence of a search term.
{-# INLINE replace1 #-}
replace1 :: String -> Fmt Builder a a -> Fmt Builder a b -> Fmt Builder a b
replace1 needle replacement =
    splitWith (breakOn needle) $ \l r0 ->
        case stripPrefix' needle r0 of
            Nothing -> fmt (fromString l)
            Just r -> fmt (fromString l) % replacement % fmt (fromString r)
  where
    breakOn pat s = go "" s
      where
        go acc [] = (reverse acc, [])
        go acc rest@(c : cs)
            | pat `isPrefixOf` rest = (reverse acc, rest)
            | otherwise = go (c : acc) cs

    stripPrefix' [] ys = Just ys
    stripPrefix' _ [] = Nothing
    stripPrefix' (x : xs) (y : ys)
        | x == y = stripPrefix' xs ys
        | otherwise = Nothing

---------------------------------------------------------------------
-- Structured output
---------------------------------------------------------------------

-- | Format a foldable as a JSON-style list.
{-# INLINE jsonList #-}
jsonList :: Foldable f => Fmt1 Builder String a -> Fmt1 Builder s (f a)
jsonList = cat1With $ \xs -> "[" ++ intercalate ", " xs ++ "]"

-- | Format a foldable as a YAML-style list.
{-# INLINE yamlList #-}
yamlList :: Foldable f => Fmt1 Builder String a -> Fmt1 Builder s (f a)
yamlList = cat1With $ \xs -> unlines (map ("- " ++) xs)

-- | Format key-value pairs as a JSON-style map.
{-# INLINE jsonMap #-}
jsonMap :: (Foldable f) => Fmt1 Builder String k -> Fmt1 Builder String v -> Fmt1 Builder s (f (k, v))
jsonMap kf vf = fmt1 $ \kvs ->
    let items = map fmtPair (toList kvs)
        fmtPair (k, v) = run1 kf k ++ ": " ++ run1 vf v
     in Builder . (++) $ "{" ++ intercalate ", " items ++ "}"

-- | Format key-value pairs as a YAML-style map.
{-# INLINE yamlMap #-}
yamlMap :: (Foldable f) => Fmt1 Builder String k -> Fmt1 Builder String v -> Fmt1 Builder s (f (k, v))
yamlMap kf vf = fmt1 $ \kvs ->
    let items = map fmtPair (toList kvs)
        fmtPair (k, v) = run1 kf k ++ ": " ++ run1 vf v
     in Builder . (++) $ unlines items
