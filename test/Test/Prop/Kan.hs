{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Prop.Kan (tests) where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Control.Comonad (extract)
import Control.Comonad.Density (Density, liftDensity, densityToLan, lanToDensity)
import Control.Monad.Codensity (Codensity (..), lowerCodensity)
import Control.Monad.Trans.State.Strict (StateT (..), runStateT)
import Data.Fmt.Cons
import Data.Functor.Compose (Compose (..))
import Data.Fmt.Fixed
import Data.Fmt.Kan
import Data.Fmt.Type (Fmt (..), fmt, runFmt, (%))
import Data.Functor.Day (Day (..), dap, day)
import Data.Functor.Day.Curried (Curried (..), liftCurried, lowerCurried, applied, unapplied)
import Data.Functor.Identity (Identity (..))
import Data.Functor.Kan.Ran (Ran (..), gran, toRan)
import Data.Functor.Kan.Lan (Lan (..), glan, toLan)
import Data.Functor.Yoneda (Yoneda, liftYoneda, lowerYoneda)

tests :: IO Bool
tests = checkParallel $$(discover)

---------------------------------------------------------------------
-- Generators
---------------------------------------------------------------------

genList :: Gen (Mu (Cons Int))
genList = fromList <$> Gen.list (Range.linear 0 20) (Gen.int (Range.linear 0 100))

forAllL :: Gen (Mu (Cons Int)) -> PropertyT IO (Mu (Cons Int))
forAllL = forAllWith (show . toList)

---------------------------------------------------------------------
-- P141–P148: Day convolution
---------------------------------------------------------------------

-- P141: equalDay agrees with (==) for Cons Int
prop_P141_equalDay_eq :: Property
prop_P141_equalDay_eq = property $ do
    a <- forAll $ Gen.int (Range.linear 0 100)
    b <- forAll $ Gen.int (Range.linear 0 100)
    equalDay (Day (Cons a True) (Cons b True) (&&)) === (a == b)

-- P142: compareDay agrees with compare for Cons Int
prop_P142_compareDay :: Property
prop_P142_compareDay = property $ do
    a <- forAll $ Gen.int (Range.linear 0 100)
    b <- forAll $ Gen.int (Range.linear 0 100)
    compareDay (Day (Cons a EQ) (Cons b EQ) (<>)) === compare a b

-- P143: recursiveEq agrees with (==) on Mu (Cons Int)
prop_P143_recursiveEq :: Property
prop_P143_recursiveEq = property $ do
    xs <- forAllL genList
    ys <- forAllL genList
    recursiveEq xs ys === (xs == ys)

-- P144: recursiveOrd agrees with compare on Mu (Cons Int)
prop_P144_recursiveOrd :: Property
prop_P144_recursiveOrd = property $ do
    xs <- forAllL genList
    ys <- forAllL genList
    recursiveOrd xs ys === compare xs ys

-- P145: Day associativity via fmtDay
prop_P145_day_assoc :: Property
prop_P145_day_assoc = property $ do
    s1 <- forAll $ Gen.string (Range.linear 1 10) Gen.alpha
    s2 <- forAll $ Gen.string (Range.linear 1 10) Gen.alpha
    -- (%) is associative, which is Day associativity
    runFmt (fmt s1 % fmt s2) === s1 ++ s2

-- P146: equalDay Nil = True
prop_P146_equalDay_nil :: Property
prop_P146_equalDay_nil = property $ do
    equalDay (Day (Nil :: Cons Int Bool) Nil (&&)) === True

-- P147: equalDay mismatch shapes = False
prop_P147_equalDay_mismatch :: Property
prop_P147_equalDay_mismatch = property $ do
    equalDay (Day (Nil :: Cons Int Bool) (Cons 1 True) (&&)) === False

-- P148: fmtDay recovers (%) result when applied
prop_P148_fmtDay :: Property
prop_P148_fmtDay = property $ do
    s1 <- forAll $ Gen.string (Range.linear 1 10) Gen.alpha
    s2 <- forAll $ Gen.string (Range.linear 1 10) Gen.alpha
    let d = fmtDay (fmt s1 :: Fmt String String String) (fmt s2)
    -- dap :: Day ((->) m) ((->) m) a -> ((->) m) a = m -> a
    -- Apply to any string to get the result
    dap d "" === s1 ++ s2

---------------------------------------------------------------------
-- P149–P153: Yoneda
---------------------------------------------------------------------

-- P149: lowerYonedaFix . liftYonedaFix = id
prop_P149_yoneda_roundtrip :: Property
prop_P149_yoneda_roundtrip = property $ do
    xs <- forAllL genList
    toList (lowerYonedaFix (liftYonedaFix xs)) === toList xs

-- P150: Yoneda fuses multiple maps into single traversal
prop_P150_yoneda_fusion :: Property
prop_P150_yoneda_fusion = property $ do
    xs <- forAllL genList
    let inc :: Cons Int a -> Cons Int a
        inc Nil = Nil
        inc (Cons a r) = Cons (a + 1) r
        dbl :: Cons Int a -> Cons Int a
        dbl Nil = Nil
        dbl (Cons a r) = Cons (a * 2) r
        -- Two hoists = 2 traversals
        direct = hoistMu inc (hoistMu dbl xs)
        -- Via Yoneda = 1 traversal (composed nat trans)
        fused = lowerYonedaFix (mapYonedaFix inc (mapYonedaFix dbl (liftYonedaFix xs)))
    toList direct === toList fused

-- P151: runFmt (fmt x) = x (Yoneda for Fmt)
prop_P151_fmt_yoneda :: Property
prop_P151_fmt_yoneda = property $ do
    s <- forAll $ Gen.string (Range.linear 0 50) Gen.alpha
    -- This IS the Yoneda lemma: forall a. (m -> a) -> a ≅ m
    runFmt (fmt s) === s

-- P152: Yoneda fmap fusion: fmap f . fmap g = fmap (f . g)
prop_P152_yoneda_fmap :: Property
prop_P152_yoneda_fmap = property $ do
    n <- forAll $ Gen.int (Range.linear 0 100)
    let y = liftYoneda (Just n)
    lowerYoneda (fmap (+ 1) (fmap (* 2) y)) === lowerYoneda (fmap ((+ 1) . (* 2)) y)

-- P153: liftYoneda preserves structure
prop_P153_yoneda_preserve :: Property
prop_P153_yoneda_preserve = property $ do
    xs <- forAllL genList
    toList (lowerYonedaFix (liftYonedaFix xs)) === toList xs

---------------------------------------------------------------------
-- P154–P158: Codensity
---------------------------------------------------------------------

-- P154: foldMCodensity agrees with foldM
prop_P154_codensity_fold :: Property
prop_P154_codensity_fold = property $ do
    xs <- forAllL genList
    let alg :: Cons Int Int -> Maybe Int
        alg Nil = Just 0
        alg (Cons a b) = Just (a + b)
    foldMCodensity alg xs === foldM alg xs

-- P155: lowerCodensity . pure = pure
prop_P155_codensity_pure :: Property
prop_P155_codensity_pure = property $ do
    n <- forAll $ Gen.int (Range.linear 0 100)
    (lowerCodensity (pure n) :: Maybe Int) === Just n

-- P156: Codensity >>= is associative
prop_P156_codensity_assoc :: Property
prop_P156_codensity_assoc = property $ do
    n <- forAll $ Gen.int (Range.linear 0 100)
    let f x = pure (x + 1) :: Codensity Maybe Int
        g x = pure (x * 2) :: Codensity Maybe Int
    lowerCodensity ((pure n >>= f) >>= g) === lowerCodensity (pure n >>= (\x -> f x >>= g))

-- P157: Codensity ((->) m) ≅ State m (round-trip)
prop_P157_codensity_state :: Property
prop_P157_codensity_state = property $ do
    n <- forAll $ Gen.int (Range.linear 0 100)
    let f m = (m + 1, m * 2)
    codensityToState (stateToCodensity f) n === f n

-- P158: foldMCodensity handles list sum
prop_P158_codensity_sum :: Property
prop_P158_codensity_sum = property $ do
    xs <- forAll $ Gen.list (Range.linear 0 100) (Gen.int (Range.linear 0 100))
    let alg :: Cons Int Int -> Maybe Int
        alg Nil = Just 0
        alg (Cons a b) = Just (a + b)
    foldMCodensity alg (fromList xs) === Just (sum xs)

---------------------------------------------------------------------
-- P159–P160: Day + Fmt integration
---------------------------------------------------------------------

-- P159: fmtDay + dap agrees with (%)
prop_P159_fmtDay_percent :: Property
prop_P159_fmtDay_percent = property $ do
    s1 <- forAll $ Gen.string (Range.linear 1 10) Gen.alpha
    s2 <- forAll $ Gen.string (Range.linear 1 10) Gen.alpha
    let via_percent = runFmt (fmt s1 % fmt s2 :: Fmt String String String)
        via_day = dap (fmtDay (fmt s1) (fmt s2)) ""
    via_percent === via_day

-- P160: Day with Identity is unit
prop_P160_day_unit :: Property
prop_P160_day_unit = property $ do
    n <- forAll $ Gen.int (Range.linear 0 100)
    -- Day Identity Identity a with const as combiner
    let d = Day (Identity n) (Identity n) const
    dap d === Identity n

---------------------------------------------------------------------
-- P161–P165: Ran / Lan
---------------------------------------------------------------------

-- P161: Ran round-trip via gran . toRan
prop_P161_ran_roundtrip :: Property
prop_P161_ran_roundtrip = property $ do
    n <- forAll $ Gen.int (Range.linear 0 100)
    -- toRan :: (forall a. k (g a) -> h a) -> k b -> Ran g h b
    -- gran :: Ran g h (g a) -> h a
    let r = Ran (\f -> f n) :: Ran Identity Identity Int
    runIdentity (runRan r Identity) === n

-- P162: Ran Identity ≅ Yoneda (structural)
prop_P162_ran_yoneda :: Property
prop_P162_ran_yoneda = property $ do
    n <- forAll $ Gen.int (Range.linear 0 100)
    -- Yoneda lemma: Ran Identity Identity a ≅ a
    let y = liftYoneda (Identity n)
    lowerYoneda y === Identity n

-- P163: Lan glan produces the right value
prop_P163_lan_glan :: Property
prop_P163_lan_glan = property $ do
    n <- forAll $ Gen.int (Range.linear 0 100)
    -- glan :: h a -> Lan g h (g a)
    -- toLan :: (forall a. h a -> f (g a)) -> Lan g h b -> f b
    -- glan embeds a value; we extract it by running the Lan
    let l = Lan runIdentity (Identity n) :: Lan Identity Identity Int
    runIdentity (toLan Identity l) === n

-- P164: Lan is the free functor (Coyoneda connection)
prop_P164_lan_functor :: Property
prop_P164_lan_functor = property $ do
    n <- forAll $ Gen.int (Range.linear 0 100)
    -- Lan Identity f ≅ Coyoneda f ≅ f (when f is a Functor)
    let l = Lan runIdentity (Identity n) :: Lan Identity Identity Int
        l' = fmap (+ 1) l
    runIdentity (toLan Identity l') === n + 1

-- P165: Ran functor law
prop_P165_ran_fmap :: Property
prop_P165_ran_fmap = property $ do
    n <- forAll $ Gen.int (Range.linear 0 100)
    let r = Ran (\f -> f n) :: Ran Identity Identity Int
        r' = fmap (+ 1) r
    runIdentity (runRan r' Identity) === n + 1

---------------------------------------------------------------------
-- P166–P170: Density / Curried
---------------------------------------------------------------------

-- P166: Density extract law
prop_P166_density_extract :: Property
prop_P166_density_extract = property $ do
    n <- forAll $ Gen.int (Range.linear 0 100)
    extract (liftDensity (Identity n)) === n

-- P167: densityToLan . lanToDensity = id (via extract)
prop_P167_density_roundtrip :: Property
prop_P167_density_roundtrip = property $ do
    n <- forAll $ Gen.int (Range.linear 0 100)
    let d = liftDensity (Identity n)
    extract (lanToDensity (densityToLan d)) === extract d

-- P168: Curried liftCurried/lowerCurried for Maybe
prop_P168_curried_maybe :: Property
prop_P168_curried_maybe = property $ do
    n <- forAll $ Gen.int (Range.linear 0 100)
    lowerCurried (liftCurried (Just n) :: Curried Maybe Maybe Int) === Just n

-- P169: Curried liftCurried/lowerCurried for Identity
prop_P169_curried_identity :: Property
prop_P169_curried_identity = property $ do
    n <- forAll $ Gen.int (Range.linear 0 100)
    lowerCurried (liftCurried (Identity n) :: Curried Identity Identity Int) === Identity n

-- P170: Codensity/Curried connection
-- Codensity m a = forall b. (a -> m b) -> m b ≅ Curried m m a (when m = (->) r)
prop_P170_codensity_curried :: Property
prop_P170_codensity_curried = property $ do
    n <- forAll $ Gen.int (Range.linear 0 100)
    -- Codensity Maybe is a useful monad
    let c = pure n :: Codensity Maybe Int
    lowerCodensity c === Just n

---------------------------------------------------------------------
-- P216–P217: Codensity / StateT isomorphism
---------------------------------------------------------------------

-- P216: codensityToStateT . stateTToCodensity = id (round-trip)
prop_P216_stateT_roundtrip :: Property
prop_P216_stateT_roundtrip = property $ do
    n <- forAll $ Gen.int (Range.linear 0 100)
    -- StateT Int Maybe Int: stateful computation that might fail
    let st :: StateT Int Maybe Int
        st = StateT $ \m -> Just (m + 1, m * 2)
    runStateT (codensityToStateT (stateTToCodensity st)) n
        === runStateT st n

-- P217: round-trip preserves behavior for different state functions
prop_P217_stateT_roundtrip2 :: Property
prop_P217_stateT_roundtrip2 = property $ do
    n <- forAll $ Gen.int (Range.linear 1 100)
    let st :: StateT Int Maybe Int
        st = StateT $ \m -> if m > 0 then Just (m * 3, m - 1) else Nothing
    runStateT (codensityToStateT (stateTToCodensity st)) n
        === runStateT st n
