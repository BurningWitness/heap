{-# LANGUAGE BangPatterns
           , CPP
           , UnboxedTuples #-}

#ifdef GENERIC
#define MODULE_NAME  Ord
#define KEY_PARAM    k
#define UNPACKED_KEY !k
#define KEY          k
#define SHOW         Show k
#define NFDATA       NFData k
#define EQ           Eq k
#define ORD          Ord k
#define IMPLY_LBRACE (
#define IMPLY_RBRACE )
#define IMPLY_AND    ,
#define IMPLIES      =>
#else
#define MODULE_NAME  KEY_TYPE
#define KEY_PARAM
#define UNPACKED_KEY {-# UNPACK #-} !KEY_TYPE
#define KEY          KEY_TYPE
#define SHOW
#define NFDATA
#define EQ
#define ORD
#define IMPLY_LBRACE
#define IMPLY_RBRACE
#define IMPLY_AND
#define IMPLIES
#endif

#ifdef GENERIC
{-|
    @t'Heap' k a@ is a spine-strict binomial min-heap with a global root
    over keys of type @('Ord' k => k)@.

    Binomial heaps have no notion of element order, as such:

    - Duplicate keys are permitted. Inserting a duplicate may put it anywhere on the
      heap, there is no guarantee a successive 'viewMin' will pick the newer entry
      over the older one.

    - Arbitrary key access has \(\mathcal{O}(n)\) time complexity
      and operations that rely on it are therefore not provided.

    == Laziness

    Evaluating the root of the heap (i.e. @(_ :: t'Heap' a)@) to
    weak head normal form evaluates the entire spine of the heap to normal form.

    Functions do not perform any additional evaluations unless
    their documentation directly specifies so.

    == Performance

    Each function's time complexity is provided in the documentation.

    \(n\) refers to the total number of entries in the heap.
    Parts of the heap are denoted using subscripts: \(n_L\) refers to the left side,
    \(n_R\) to the right side, and \(n_M\) to entries collected with the use of a 'Monoid'.

    == Implementation

    See the implementation section in "Data.Heap.Ord.Unsafe" for the explanation
    of the innerworkings.

    Description of the binomial heap and the global root transformation can be found
    within the following papers:

    - Chris Okasaki, /"Purely Functional Data Structures"/, September 1996, pages 68-72,
      https://www.cs.cmu.edu/~rwh/students/okasaki.pdf

    - Gerth Stølting Brodal and Chris Okasaki, /"Optimal Purely Functional Priority Queues"/
      , October 1996, pages 5-8 and 14-16,
      https://citeseerx.ist.psu.edu/document?doi=59776eb2358b4be265e7dae9df4f946dab25bf6f
-}
#else
{-|
    This module (and every module below it) is a duplicate of "Data.Heap.Ord",
    specialized to keys of type KEY.
-}
#endif

module Data.Heap.MODULE_NAME
  ( -- * Itself
    Heap (Empty, (:<|))

    -- * Construct
  , empty
  , singleton

    -- * Single-key
    -- ** Insert
  , insert

    -- * Minimal-key
    -- ** Lookup
  , lookupMin
  , Lookup (..)
  , lookupMinWithKey

    -- ** Map
  , adjustMin
  , adjustMin'

  , adjustMinWithKey
  , adjustMinWithKey'

    -- ** Delete
  , deleteMin

    -- ** Update
  , updateMin
  , updateMinWithKey

    -- ** View
  , ViewMin (..)
  , viewMin

    -- * Full heap
    -- ** Size
  , Data.Heap.MODULE_NAME.Internal.null
  , size

    -- ** Map
  , Data.Heap.MODULE_NAME.Internal.map
  , map'
  , mapWithKey
  , mapWithKey'
 
    -- ** Fold
    -- | === Left-to-right
  , Data.Heap.MODULE_NAME.Internal.foldl
  , Data.Heap.MODULE_NAME.Internal.foldl'
  , foldlWithKey
  , foldlWithKey'

    -- | === Right-to-left
  , Data.Heap.MODULE_NAME.Internal.foldr
  , Data.Heap.MODULE_NAME.Internal.foldr'
  , foldrWithKey
  , foldrWithKey'

    -- | === Monoid
  , Data.Heap.MODULE_NAME.Internal.foldMap
  , foldMapWithKey

    -- ** Traverse
  , Data.Heap.MODULE_NAME.Internal.traverse
  , traverseWithKey

    -- ** Union
  , union

  ) where

import           Data.Heap.MODULE_NAME.Internal
