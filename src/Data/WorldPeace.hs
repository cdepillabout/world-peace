{- |
Module      :  Data.WorldPeace

Copyright   :  Dennis Gosnell 2017
License     :  BSD3

Maintainer  :  Dennis Gosnell (cdep.illabout@gmail.com)
Stability   :  experimental
Portability :  unknown

-}

module Data.WorldPeace
  (
  -- *** 'OpenUnion'
    OpenUnion
  -- **** 'OpenUnion' Helpers
  , openUnion
  , fromOpenUnion
  , fromOpenUnionOr
  , openUnionPrism
  , openUnionLift
  , openUnionMatch
  , catchesOpenUnion
  -- **** 'Union' (used by 'OpenUnion')
  -- | 'OpenUnion' is a type synonym around 'Union'. Most users will be able to
  -- work directly with 'OpenUnion' and ignore this 'Union' type.
  , Union(..)
  -- ***** Union helpers
  , union
  , absurdUnion
  , umap
  , catchesUnion
  -- ***** Union optics
  , _This
  , _That
  -- ***** Typeclasses used with Union
  , Nat(Z, S)
  , RIndex
  , UElem(..)
  , IsMember
  -- **** 'OpenProduct' (used by 'OpenUnion')
  -- | This 'Product' type is used to easily create a case-analysis for
  -- 'Union's.  You can see it being used in 'catchesOpenUnion' and
  -- 'catchesEnvelope'.  The 'ToProduct' type class makes it easy to convert a
  -- tuple to a 'Product'.  This makes it so the end user only has to worry
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
