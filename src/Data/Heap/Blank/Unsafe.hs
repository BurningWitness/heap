{-# LANGUAGE BangPatterns
           , CPP
           , UnboxedTuples #-}

{-# OPTIONS_HADDOCK not-home #-}

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
{-| Data structure internals, helper operations and unsafe functions.

    == Implementation

    The implementation mostly copies the heap described by Okasaki
    (and on the [Wikipedia page](https://en.wikipedia.org/w/index.php?title=Binomial_heap&oldid=1221086681#Binomial_heap)). There are two differences:

    1. Ranks are not stored in nodes; for the top-level forest they're instead
       inferred from heap's size. All other ranks are redundant information, as pointed
       out by Okasaki on page 70 of his /"Purely Functional Data Structures/" paper.

    2. The heap has a global root for \(\mathcal{O}(1)\) minimal-entry access.
       This transformation does not cost anything as the heap already has to hold
       its size at the root to satisfy the first point.

    Operations are therefore altered appropriately:

    - /insert/

        The number of top-level trees touched by the insertion is the same
        as the number of consecutive lowermost @1@ bits in the size of the tree.

    - /meld/

        Ranks and whichever heap will bottom out first is known beforehand from the sizes.
        Melding continues until the smaller heap contains no more ranks, the remaining
        carry bit (if any) can then be inserted into the remainder of the larger heap.

    - /delete-min/

        Given a heap \(A\), deletion of the node with the minimal key leaves a forest \(B\)
        which needs to be reversed and melded with the remaining heap. \(B\)
        is guaranteed to have the size of \(2^k - 1\) where \(k \ge 1\). \(A\)
        is known to be larger than \(B\). Any carry during the meld results in
        repeated carries for the rest of the operation. The deleted node is guaranteed
        to be the lowermost node in what remains of \(A\) after the meld; the remaining
        carry bit (if any) will have the same rank as the deleted node.

 -}
#else
{-| Data structure internals, helper operations and unsafe functions.
 -}
#endif

module Data.Heap.MODULE_NAME.Unsafe
  ( Size
  , Heap (Heap, None)
  , Forest (..)
  ) where

import           Data.Heap.MODULE_NAME.Internal
