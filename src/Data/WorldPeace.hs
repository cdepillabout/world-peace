{- |
Module      :  Data.WorldPeace

Copyright   :  Dennis Gosnell 2017
License     :  BSD3

Maintainer  :  Dennis Gosnell (cdep.illabout@gmail.com)
Stability   :  experimental
Portability :  unknown

This package defines a type called 'OpenUnion'. This represents an open union
of possible types (also called an open sum type).

Here is an example of taking a 'String', and lifting it up into an open union
of a 'String' and 'Int':

@
  let int = 3 :: 'Int'
  let o = 'openUnionLift' int :: 'OpenUnion' \'['String', 'Int']
@

There are a couple different ways to pattern match on a 'OpenUnion'.

The easiest one is to use 'catchesOpenUnion', which takes a tuple of handlers for
each possible type in the 'OpenUnion':

@
  let strHandler = (\str -> \"got a String: \" '++' str) :: 'String' -> 'String'
      intHandler = (\int -> \"got an Int: \" '++' 'show' int) :: 'Int' -> 'String'
  in 'catchesOpenUnion' (strHandler, intHandler) u :: 'String'
@

The above will print @got an Int: 3@.

There is also the 'openUnionMatch' function, as well as 'fromOpenUnion' and
'openUnion'. Read the documentation below for more information.
-}

module Data.WorldPeace
  (
  -- * 'OpenUnion'
    OpenUnion
  -- ** 'OpenUnion' Helpers
  , openUnion
  , fromOpenUnion
  , fromOpenUnionOr
  , openUnionPrism
  , openUnionLift
  , openUnionMatch
  , catchesOpenUnion
  , IsMember
  -- ** 'Union' (used by 'OpenUnion')
  -- | 'OpenUnion' is a type synonym around 'Union'. Most users will be able to
  -- work directly with 'OpenUnion' and ignore this 'Union' type.
  , Union(..)
  -- *** Union helpers
  , union
  , absurdUnion
  , umap
  , catchesUnion
  -- *** Union optics
  , _This
  , _That
  -- *** Typeclasses used with Union
  , Nat(Z, S)
  , RIndex
  , UElem(..)
  -- ** 'OpenProduct'
  -- | This 'OpenProduct' type is used to easily create a case-analysis for
  -- 'Union's.  You can see it being used in 'catchesOpenUnion' and
  -- The 'ToProduct' type class makes it easy to convert a
  -- tuple to a 'Product'.  This class is used so that the end user only has to worry
  -- about working with tuples, and can mostly ignore this 'Product' type.
  , OpenProduct
  , Product(..)
  , ToOpenProduct
  , tupleToOpenProduct
  , ToProduct
  , tupleToProduct
  , ReturnX
  ) where

import Data.WorldPeace.Product
import Data.WorldPeace.Union
