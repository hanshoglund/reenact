
-------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : GPL
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : stable
-- Portability : portable
--
-- Utility functions.
--
-------------------------------------------------------------------------------------    

module Control.Reactive.Util (
        -- ** String and Char
        toUpperChar,
        toLowerChar,
        toUpperString,
        toLowerString,
        toCapitalString,       
        isSubstringOf,
        isInfixOfNormalized,
        isSubstringOfNormalized,
        
        -- ** Lists
        prefix,
        suffix,
        sep,
        pre,
        post,
        wrap,
        concatSep,
        concatPre,
        concatPost,
        concatWrap,
        concatLines,
        divideList,
        breakList, 
        
        -- ** Monads
        concatMapN,
        concatMapM,
        concatMapA,
        
        -- ** Special map
        mapIndexed,      
        
        -- ** Mathematics
        tau,             
        
        -- ** System
        -- execute,
  ) where

import Prelude hiding (concat)

import Data.Monoid             
import Control.Applicative
import Data.Foldable
import Data.Traversable
import Control.Monad (MonadPlus)

-- import System.Posix

import qualified Data.Char as Char
import qualified Data.List as List

-------------------------------------------------------------------------------------
-- String and Char
-------------------------------------------------------------------------------------

-- |
-- Synonym for 'Char.toUpper'
toUpperChar :: Char -> Char
toUpperChar = Char.toUpper

-- |
-- Synonym for 'Char.toLower'
toLowerChar :: Char -> Char
toLowerChar = Char.toLower

-- |
-- Synonym for 'fmap Char.toUpper'
toUpperString :: String -> String
toUpperString = fmap Char.toUpper

-- |
-- Synonym for 'fmap Char.toLower'
toLowerString :: String -> String
toLowerString = fmap Char.toLower

-- |
-- Convert a string to use upper case for the leading letter and lower case for
-- remaining letters.
toCapitalString :: String -> String
toCapitalString [] = []
toCapitalString (x:xs) = toUpperChar x : toLowerString xs

isSubstringOf :: String -> String -> Bool
a `isSubstringOf` b 
    =  a `List.isPrefixOf` b
    || a `List.isInfixOf`  b
    || a `List.isSuffixOf` b

isInfixOfNormalized :: String -> String -> Bool
a `isInfixOfNormalized` b = toLowerString a `List.isInfixOf` toLowerString b 

isSubstringOfNormalized :: String -> String -> Bool
a `isSubstringOfNormalized` b = toLowerString a `isSubstringOf` toLowerString b 

-------------------------------------------------------------------------------------
-- List
-------------------------------------------------------------------------------------

-- |
-- Synonym for '(++)'
--
prefix :: [a] -> [a] -> [a]
prefix x = (x ++)

-- |
-- Synonym for 'flip (++)'
--
suffix :: [a] -> [a] -> [a]
suffix x = (++ x)

-- |
-- Separate a list by the given element.
--
-- Equivalent to 'List.intersperse'
--
sep :: a -> [a] -> [a]
sep = List.intersperse

-- |
-- Initiate and separate a list by the given element.
--
pre :: a -> [a] -> [a]
pre x = (x :) . sep x

-- |
-- Separate and terminate a list by the given element.
--
post :: a -> [a] -> [a]
post x = suffix [x] . sep x

-- |
-- Separate and terminate a list by the given element.
--
wrap :: a -> a -> [a] -> [a]
wrap x y = (x :) . suffix [y] . sep x

-- |
-- Combination of 'concat' and 'sep'.
--
concatSep :: [a] -> [[a]] -> [a]
concatSep x = concat . sep x

-- |
-- Combination of 'concat' and 'pre'.
--
concatPre :: [a] -> [[a]] -> [a]
concatPre x = concat . pre x

-- |
-- Combination of 'concat' and 'post'.
--
concatPost :: [a] -> [[a]] -> [a]
concatPost x = concat . post x

-- |
-- Combination of 'concat' and 'wrap'.
--
concatWrap :: [a] -> [a] -> [[a]] -> [a]
concatWrap x y = concat . wrap x y


concatLines :: [String] -> String
concatLines = concatPost "\n"

-- |
-- Divide a list into parts of maximum length n.
--
divideList :: Int -> [a] -> [[a]]
divideList n xs
    | length xs <= n = [xs]
    | otherwise = [take n xs] ++ (divideList n $ drop n xs)

-- |
-- Break up a list into parts of maximum length n, inserting the given list as separator.
-- Useful for breaking up strings, as in @breakList 80 "\n" str@.
--
breakList :: Int -> [a] -> [a] -> [a]
breakList n z = mconcat . List.intersperse z . divideList n


-- |
-- A version of 'concatMap' generalized to arbitrary 'Monoid' instances.
--
concatMapN :: (Applicative f, Monoid b) => (a -> f b) -> [a] -> f b
concatMapN f = fmap mconcat . traverse f

-- |
-- A version of 'concatMap' generalized to arbitrary 'MonadPlus' instances.
--
concatMapM :: (MonadPlus m, Applicative f, Traversable t) => (a -> f (m b)) -> t a -> f (m b)
concatMapM f = fmap msum . traverse f

-- |
-- A version of 'concatMap' generalized to arbitrary 'Alternative' instances.
--
concatMapA :: (Alternative m, Applicative f, Traversable t) => (a -> f (m b)) -> t a -> f (m b)
concatMapA f = fmap asum . traverse f


mapIndexed :: (Int -> a -> b) -> [a] -> [b]
mapIndexed f as = map (uncurry f) (zip is as)
    where
        n  = length as - 1
        is = [0..n]

tau :: Floating a => a
tau = 2 * pi

{-
-- |
-- Excecute an external process asynchronously (using @forkProcess@) with the given arguments.
--
execute :: FilePath -> [String] -> IO ()
execute program args = do
    forkProcess $ executeFile program True args Nothing
    return ()
-}


