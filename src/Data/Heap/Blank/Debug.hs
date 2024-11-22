{-# LANGUAGE BangPatterns
           , CPP #-}

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

{-| Safe functions for datatype introspection.
 -}

module Data.Heap.MODULE_NAME.Debug
  ( -- * Show
    showsTree

    -- * Validate
  , Validity (..)
  , Reason (..)
  , validate
  ) where

import           Data.Heap.MODULE_NAME.Internal

import           Data.Bits



-- | \(\mathcal{O}(n)\). Shows the internal structure of the heap.
#ifdef GENERIC
showsTree :: (k -> ShowS) -> (a -> ShowS) -> Heap k a -> ShowS
showsTree ks s h0 =
#else
showsTree :: (a -> ShowS) -> Heap a -> ShowS
showsTree s h0 =
#endif
  case h0 of
    None          -> showString "None"
    Heap n kx x t ->
      showString "Heap " . shows n
          . showChar ' '
#ifdef GENERIC
                         . ks kx
#else
                         . shows kx
#endif
          . showChar ' ' . s x

         . showChar '\n' . go 2 t
  where
    go i h =
      mappend (replicate i ' ') .
        case h of
          Tree k x f h' ->
            showString "Tree "
#ifdef GENERIC
                               . ks k
#else
                               . shows k
#endif
                . showChar ' ' . s x
               . showChar '\n' . go (i + 2) f
               . showChar '\n' . go i h'

          Nil             -> showString "Nil"



-- | Whether the heap is well-formed.
data Validity KEY_PARAM
                = Valid
                | Invalid (Reason KEY_PARAM)
                  deriving Show

-- | Reason for why the heap is considered malformed.
data Reason KEY_PARAM
              = -- | Forest below the node with this key holds a smaller key.
                KeyBelow UNPACKED_KEY

                -- | Rank below the node with this key is malformed.
              | MalformedRank UNPACKED_KEY

                -- | Stored ranks are not monotonically ascending.
              | NotMonotone ![Int]

                -- | Stored ranks do not match the size.
              | RankMismatch {-# UNPACK #-} !Size ![Int]
                deriving Show


-- | \(\mathcal{O}(n)\). Checks whether the heap is well-formed.
validate :: ORD IMPLIES Heap KEY_PARAM a -> Validity KEY_PARAM
validate None               = Valid
validate (Heap n0 km0 _ t0) =
  case checkKeys km0 t0 of
    Invalid r -> Invalid r
    Valid     -> checkRanks n0 t0



checkKeys :: ORD IMPLIES KEY -> Forest KEY_PARAM a -> Validity KEY_PARAM
checkKeys !_  Nil               = Valid
checkKeys  ka (Tree kb _ fb ff)
  | kb < ka                      = Invalid $ KeyBelow ka
  | Invalid r <- checkKeys kb fb = Invalid r
  | otherwise                    = checkKeys ka ff



checkRanks :: Word -> Forest KEY_PARAM a -> Validity KEY_PARAM
checkRanks n0 fs0 =
  case go fs0 of
    Left r   -> Invalid r
    Right is
      | not $ isMonoAsc is -> Invalid $ NotMonotone is
      | n0 /= crunch is    -> Invalid $ RankMismatch n0 is
      | otherwise          -> Valid
  where
    go Nil              = Right []
    go (Tree _ _ fa ff) = do
      n <- checkRank fa
      (:) n <$> go ff



isMonoAsc :: [Int] -> Bool
isMonoAsc []     = True
isMonoAsc (a:bs) = go a bs
  where
    go !_    []  = True
    go  x (y:zs) = x < y && go y zs

crunch :: [Int] -> Word
crunch    []  = 0
crunch (x:ys) = unsafeShiftL 1 x + crunch ys



-- Check that a single tree is well-formed
checkRank :: Forest KEY_PARAM a -> Either (Reason KEY_PARAM) Int
checkRank Nil               = Right 0
checkRank (Tree kb _ fb ff) = do
  n <- checkRank fb
  m <- checkRank ff
  if n /= m
    then Left $ MalformedRank kb
    else Right $! n + 1
