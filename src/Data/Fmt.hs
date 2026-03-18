-- | Type-safe string formatting and pretty-printing.
--
-- This module re-exports the most commonly used parts of the
-- library. For specialized functionality, import the submodules
-- directly:
--
-- * "Data.Fmt.Type" — core @Fmt@ type and combinators
-- * "Data.Fmt.Tree" — pretty-printing: smart constructors, layout, rendering
-- * "Data.Fmt.Code" — numeric\/binary encoders (@d@, @x@, @s@, etc.)
-- * "Data.Fmt.ByteString" — @ByteFmt@, @runByteFmt@, @printf@
-- * "Data.Fmt.Text" — @TextFmt@, @runTextFmt@
-- * "Data.Fmt.String" — @StringFmt@, @runStringFmt@
-- * "Data.Fmt.Fixed" — fixed points (@Mu@, @Fix@, @Nu@) and recursion schemes
-- * "Data.Fmt.Cons" — @Cons@ pattern functor, streaming metamorphisms
-- * "Data.Fmt.Functor" — @Doc@ pattern functor, @Tree@ type alias
-- * "Data.Fmt.Kan" — Kan extension connections (Day, Yoneda, Codensity, etc.)
module Data.Fmt (
    -- * Core Fmt type (from Data.Fmt.Type)
    Fmt (..),
    runFmt,
    Fmt1,
    Fmt2,
    Fmt3,
    fmt1,
    fmt2,
    fmt1_,
    fmt2_,
    (.%),
    cat1,
    fmt,
    (%),
    apply,
    bind,
    refmt,
    prefix,
    suffix,
    enclose,
    tuple,
    quotes,
    quotes',
    parens,
    braces,
    brackets,
    backticks,
    left1,
    right1,
    either1,
    maybe1,

    -- * Tree type (from Data.Fmt.Functor)
    Tree,

    -- * Pretty-printing (from Data.Fmt.Tree)
    -- ** Smart constructors
    emptyDoc,
    leaf,
    hardline,
    line,
    line',
    softline,
    softline',
    flatAlt,
    nest,
    group,
    annotate,
    column,
    nesting,

    -- ** Separators
    (<+>),
    hsep,
    vsep,
    sep,
    hcat,
    vcat,
    cat,
    fillSep,
    fillCat,

    -- ** Indentation
    align,
    hang,
    indent,

    -- ** Enclosure
    list,
    tupled,
    encloseSep,
    surround,
    punctuate,

    -- ** Layout and rendering
    LayoutOptions (..),
    PageWidth (..),
    defaultLayoutOptions,
    pretty,

    -- * Numeric encoders (from Data.Fmt.Code)
    c,
    s,
    d,
    u,
    x,
    e,
    f,
    g,
) where

import Data.Fmt.Type hiding (cat, indent)
import Data.Fmt.Functor (Tree)
import Data.Fmt.Tree
    ( emptyDoc, leaf, hardline, line, line', softline, softline'
    , flatAlt, nest, group, annotate, column, nesting
    , (<+>), hsep, vsep, sep, hcat, vcat, cat, fillSep, fillCat
    , align, hang, indent
    , list, tupled, encloseSep, surround, punctuate
    , LayoutOptions(..), PageWidth(..), defaultLayoutOptions, pretty
    )
import Data.Fmt.Code (c, s, d, u, x, e, f, g)
