{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.Exception.Assert
    ( module Control.Exception.Assert
    , assert
    ) where

import Prelude
#if !MIN_VERSION_base(4,6,0)
    hiding (catch)
#endif
import Control.Applicative
import Control.Exception
import Data.Data

-- | A distict 'Exception' from 'AssertionFailed', so that we stop fudging
-- the exception message after the first 'mapException'.
--
-- The name comes from abbreviating ‘assert’ and translating the result to
-- my native tongue.
newtype Arse = Arse String deriving (Typeable)
instance Show Arse where show (Arse s) = s
instance Exception Arse where
    fromException se = do
        AssertionFailed failure <- fromException se
        return (Arse failure)

-- | Generic helper for 'assert' that maps 'AssertFailure' 'Exception's to
-- 'Arse', adding a descriptive message along the way. Use this to build
-- your own 'assert' helpers, such as 'byOrd'. A rule is included which
-- rewrites 'assertMessage' to 'id' when compiling with @-fignore-asserts@.
{-# INLINE [1] assertMessage #-}
{-# RULES "assertMessage" forall name msg.
    assertMessage name msg (\x -> x) = id #-}
assertMessage :: String -> String -> (a -> a) -> a -> a
assertMessage name msg arse = mapException describe . arse where
    describe (AssertionFailed failure) = Arse $
        oneline failure ++ " \"" ++ name ++ "\", " ++ msg
    oneline = filter ((&&) <$> (/=) '\n' <*> (/=) '\r')

-- | Assert that two values are equal.
--
-- >>> byEq assert "Bool" False True ()
-- *** Exception: … Assertion failed "Bool", False ≠ True
{-# INLINE byEq #-}
byEq :: (Eq x, Show x) => (Bool -> a -> a) -> String ->
    x -> x -> a -> a
byEq arse name x y = assertMessage name
    (show x ++ " ≠ " ++ show y) (arse $ x == y)

-- | Assert that two values obey the given 'Ordering'.
--
-- >>> byOrd assert "Int" LT 0 1 ()
-- ()
{-# INLINE byOrd #-}
byOrd :: (Ord x, Show x) => (Bool -> a -> a) -> String ->
    Ordering -> x -> x -> a -> a
byOrd arse name o x y = assertMessage name
    (show x ++ no ++ show y) (arse $ o == compare x y)
  where
    no = case o of
        LT -> " ≮ "
        EQ -> " ≠ "
        GT -> " ≯ "

-- | Assert that a value satisfies the given predicate.
--
-- >>> byPred assert "Odd" odd 4 ()
-- *** Exception: … Assertion failed "Odd", 4
{-# INLINE byPred #-}
byPred :: (Show x) => (Bool -> a -> a) -> String ->
    (x -> Bool) -> x -> a -> a
byPred arse name p x = assertMessage name (show x) (arse $ p x)

