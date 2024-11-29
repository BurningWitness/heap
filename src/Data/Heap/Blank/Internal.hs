{-# LANGUAGE BangPatterns
           , CPP
           , PatternSynonyms
           , UnboxedTuples
           , ViewPatterns #-}

#define SINGLE_QUOTE '

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

module Data.Heap.MODULE_NAME.Internal
  ( Heap (Empty, (:<|), ..)
  , Forest (..)
  , Size

  , empty
  , singleton

  , insert

  , union

  , Lookup (..)
  , lookupMin
  , lookupMinWithKey

  , adjustMin
  , adjustMin'
  , adjustMinWithKey
  , adjustMinWithKey'

  , deleteMin

  , updateMin
  , updateMinWithKey

  , ViewMin (..)
  , viewMin

  , Data.Heap.MODULE_NAME.Internal.null
  , size

  , map
  , map'
  , mapWithKey
  , mapWithKey'

  , Data.Heap.MODULE_NAME.Internal.foldl
  , Data.Heap.MODULE_NAME.Internal.foldl'
  , foldlWithKey
  , foldlWithKey'

  , Data.Heap.MODULE_NAME.Internal.foldr
  , Data.Heap.MODULE_NAME.Internal.foldr'
  , foldrWithKey
  , foldrWithKey'

  , Data.Heap.MODULE_NAME.Internal.foldMap
  , foldMapWithKey

  , Data.Heap.MODULE_NAME.Internal.traverse
  , traverseWithKey
  ) where

import           Data.Heap.Common.Internal

import           Control.Applicative (liftA2, liftA3)
import           Control.DeepSeq
import           Data.Bits
import           Data.Foldable
import           Data.Functor.Classes
import           Prelude hiding (map)



-- | Spine-strict heap.
data Heap KEY_PARAM a =
       Heap
         {-# UNPACK #-} !Size
         UNPACKED_KEY          -- ^ Smallest key in the heap.
         a                     -- ^ Value under the smallest key.
         !(Forest KEY_PARAM a)

       | None

{-# COMPLETE Empty, (:<|) #-}

-- | \(\mathcal{O}(1)\). Bidirectional pattern synonym matching an empty heap.
pattern Empty :: Heap KEY_PARAM a
pattern Empty <- (Data.Heap.MODULE_NAME.Internal.null -> True)
  where
    Empty = empty

infixr 5 :<|
-- | \(\mathcal{O}(1)\).
--   Unidirectional pattern synonym analyzing the minimum of a non-empty heap.
pattern (:<|)
  :: ORD
  IMPLIES Lookup KEY_PARAM a
     -- | \(\mathcal{O}(\log n)\) thunk.
  -> Heap KEY_PARAM a
  -> Heap KEY_PARAM a
pattern l :<| h <- (viewMin -> Just (l :< h))



-- | A forest is a cons-list of binomial trees, such that:
--
--   - rank progression on each level is strictly monotonic;
--
--   - keys on subsequent levels are greater than or equal to the key of the node above.
--
--   The top-level forest has its ranks in ascending order; the ranks
--   correspond to the base-2 powers in the 'Size' of the heap.
--
--   Forests on every other level have their ranks in descending order;
--   the ranks are packed densely. As a consequence, for any non-top-level tree
--   of rank \(k\), except for \(k = 0\), the next tree and
--   the largest tree below are both of rank \(k - 1\).
data Forest KEY_PARAM a
                = Tree
                    UNPACKED_KEY
                    a
                    !(Forest KEY_PARAM a) -- ^ Forest below this node.
                    !(Forest KEY_PARAM a) -- ^ Next tree in the forest.

                | Nil


instance (Show a IMPLY_AND SHOW IMPLY_AND ORD) => Show (Heap KEY_PARAM a) where
  showsPrec = Data.Functor.Classes.liftShowsPrec showsPrec showList

instance IMPLY_LBRACE SHOW IMPLY_AND ORD IMPLY_RBRACE IMPLIES Show1 (Heap KEY_PARAM) where
  liftShowsPrec sf sl _ h0 =
    showChar '[' . ( case h0 of
                       Empty              -> id
                       Lookup kx x :<| h' ->
                           Data.Functor.Classes.liftShowsPrec sf sl 11 (kx, x)
                         . go h'
                   )
                 . showChar ']'
    where
      go h =
        case h of
          Empty              -> id
          Lookup km m :<| ff ->
            showChar ',' . Data.Functor.Classes.liftShowsPrec sf sl 11 (km, m) . go ff


instance (NFData a IMPLY_AND NFDATA) => NFData (Heap KEY_PARAM a) where
#ifdef GENERIC
  rnf = liftRnf2 rnf rnf

instance NFDATA IMPLIES NFData1 (Heap KEY_PARAM) where
  liftRnf = liftRnf2 rnf

instance NFData2 Heap where
  liftRnf2 _   _  None            = ()
  liftRnf2 knf nf (Heap _ km m t) = knf km `seq` nf m `seq` go t
    where
      go Nil               = ()
      go (Tree ka a fa ff) = knf ka `seq` nf a `seq` go fa `seq` go ff
#else
  rnf = liftRnf rnf

instance NFData1 (Heap KEY_PARAM) where
  liftRnf _  None           = ()
  liftRnf nf (Heap _ _ m t) = nf m `seq` go t
    where
      go Nil              = ()
      go (Tree _ a fa ff) = nf a `seq` go fa `seq` go ff
#endif


instance ORD IMPLIES Semigroup (Heap KEY_PARAM a) where
  (<>) = union

instance ORD IMPLIES Monoid (Heap KEY_PARAM a) where
  mempty = empty


-- | Uses 'map'.
instance Functor (Heap KEY_PARAM) where
  fmap = Data.Heap.MODULE_NAME.Internal.map

-- | The order of entries is arbitrary, except for the leftmost entry, which is
--   guaranteed to have the smallest key.
instance Foldable (Heap KEY_PARAM) where
  foldl   = Data.Heap.MODULE_NAME.Internal.foldl
  foldl'  =
    Data.Heap.MODULE_NAME.Internal.foldl'

  foldr   = Data.Heap.MODULE_NAME.Internal.foldr
  foldr'  =
    Data.Heap.MODULE_NAME.Internal.foldr'

  foldMap = Data.Heap.MODULE_NAME.Internal.foldMap

  null = Data.Heap.MODULE_NAME.Internal.null

  length = fromIntegral . Data.Heap.MODULE_NAME.Internal.size

instance Traversable (Heap KEY_PARAM) where
  traverse = Data.Heap.MODULE_NAME.Internal.traverse



-- | \(\mathcal{O}(1)\). Empty heap.
empty :: Heap KEY_PARAM a
empty = None

-- | \(\mathcal{O}(1)\). Heap with a single entry.
singleton :: KEY -> a -> Heap KEY_PARAM a
singleton kx x = Heap 0 kx x Nil



-- | \(\mathcal{O}(\log n)\). Insert a value in the heap with a given key.
insert :: ORD IMPLIES KEY -> a -> Heap KEY_PARAM a -> Heap KEY_PARAM a
insert !kx x None            = singleton kx x
insert  kx x (Heap n ka a t) =
  let !(# km, m, ky, y #) =
        if kx < ka
          then (# kx, x, ka, a #)
          else (# ka, a, kx, x #)

      n' = n + 1

  in Heap n' km m $ insertF n ky y Nil t

insertF
  :: ORD IMPLIES Word -> KEY -> a -> Forest KEY_PARAM a -> Forest KEY_PARAM a -> Forest KEY_PARAM a
insertF n !ka a !fa fs =
  case n .&. 1 of
    0 -> Tree ka a fa fs
    _ -> case fs of
           Nil             -> Nil -- impossible
           Tree kb b fb ff ->
             let !(# km, m, fm #) =
                   if ka < kb
                     then (# ka, a, Tree kb b fb fa #)
                     else (# kb, b, Tree ka a fa fb #)

                 !n' = unsafeShiftR n 1

             in insertF n' km m fm ff



-- | Key together with the value.
data Lookup KEY_PARAM a
                = Lookup
                    UNPACKED_KEY
                    a
                  deriving (Show, Eq)

-- | View of the minimum of the heap.
data ViewMin KEY_PARAM a
                 = (:<)
                     {-# UNPACK #-} !(Lookup KEY_PARAM a)

                     -- | \(\mathcal{O}(\log n)\) thunk.
                     (Heap KEY_PARAM a)
                   deriving Show

{-# INLINE viewMin #-}
-- | \(\mathcal{O}(1)\).
--   Analyze the minimum of the heap.
viewMin :: ORD IMPLIES Heap KEY_PARAM a -> Maybe (ViewMin KEY_PARAM a)
viewMin None             = Nothing
viewMin (Heap n ka a fs) = Just $! Lookup ka a :< deleteMin1 n fs

{-# INLINEABLE deleteMin #-}
-- | \(\mathcal{O}(\log n)\).
--   Delete a value at the smallest key in the heap.
deleteMin :: ORD IMPLIES Heap KEY_PARAM a -> Heap KEY_PARAM a
deleteMin None            = None
deleteMin (Heap n _ _ fs) = deleteMin1 n fs

{-# NOINLINE deleteMin1 #-}
deleteMin1 :: ORD IMPLIES Size -> Forest KEY_PARAM a -> Heap KEY_PARAM a
deleteMin1 !_ Nil                  = None
deleteMin1  n fs@(Tree ka a fa ff) =
  let !(# km, m, fm #) = minKeyF ka a fa ff
  in Heap (n - 1) km m $ biasedMeldF n fs (reverseF fm)



-- Find the smallest key in the forest.
minKeyF
  :: ORD
  IMPLIES KEY -> a -> Forest KEY_PARAM a -> Forest KEY_PARAM a
  -> (# KEY, a, Forest KEY_PARAM a #)
minKeyF !ka a !fa Nil               = (# ka, a, fa #)
minKeyF  ka a  fa (Tree kb b fb ff) =
  let !(# km, m, fm #) = if kb < ka
                          then (# kb, b, fb #)
                          else (# ka, a, fa #)

  in minKeyF km m fm ff



-- Reverse the tree order in a forest.
reverseF :: ORD IMPLIES Forest KEY_PARAM a -> Forest KEY_PARAM a
reverseF = go Nil
  where
    go !rs Nil               = rs
    go  rs (Tree ka a fa ff) = go (Tree ka a fa rs) ff



-- Meld two forests, knowing that the second one is either (2^n - 1) or 0
biasedMeldF
  :: ORD
  IMPLIES Word -> Forest KEY_PARAM a -> Forest KEY_PARAM a -> Forest KEY_PARAM a
biasedMeldF !n !rs fs =
  case fs of
    Nil             -> case rs of
                         Nil           -> Nil -- impossible
                         Tree _ _ _ rr -> rr
    Tree ka a fa ff ->
      let !n' = unsafeShiftR n 1
      in case n .&. 1 of
           0 -> Tree ka a fa $ biasedMeldF n' rs ff
           _ -> case rs of
                  Nil             -> Nil -- impossible
                  Tree kb b fb rr ->
                    let !(# km, m, fm #) =
                          if kb < ka
                            then (# kb, b, Tree ka a fa fb #)
                            else (# ka, a, Tree kb b fb fa #)

                    in biasedMeldCarryF n' km m fm rr ff


biasedMeldCarryF
  :: ORD
  IMPLIES Word
  -> KEY -> a -> Forest KEY_PARAM a -> Forest KEY_PARAM a -> Forest KEY_PARAM a
  -> Forest KEY_PARAM a
biasedMeldCarryF !n !kc c !fc !rs fs =
  case fs of
    Nil             -> Tree kc c fc $ case rs of
                                        Nil           -> Nil -- impossible
                                        Tree _ _ _ rr -> rr
    Tree ka a fa ff ->
      let !n' = unsafeShiftR n 1
      in case n .&. 1 of
           0 -> let !(# km, m, fm #) =
                      if ka < kc
                        then (# ka, a, Tree kc c fc fa #)
                        else (# kc, c, Tree ka a fa fc #)

                in biasedMeldCarryF n' km m fm rs ff

           _ -> Tree kc c fc $
                  case rs of
                    Nil             -> Nil -- impossible
                    Tree kb b fb rr ->
                      let !(# km, m, fm #) =
                            if kb < ka
                              then (# kb, b, Tree ka a fa fb #)
                              else (# ka, a, Tree kb b fb fa #)

                      in biasedMeldCarryF n' km m fm rr ff



-- | \(\mathcal{O}(\log (\max (n_A, n_B))\). Union of two heaps.
union :: ORD IMPLIES Heap KEY_PARAM a -> Heap KEY_PARAM a -> Heap KEY_PARAM a
union None rs = rs
union fs None = fs
union (Heap na ka a ta) (Heap nb kb b tb) =
  let !(# km, m, ky, y #) =
        if kb < ka
          then (# kb, b, ka, a #)
          else (# ka, a, kb, b #)

      !(# nr, rs, nf, fs #) =
        if nb < na
          then (# nb, tb, na, ta #)
          else (# na, ta, nb, tb #)

  in Heap (na + nb + 1) km m $ meldCarryF nf nr ky y Nil fs rs


-- Meld two forests, knowing that the second one is not greater in size
meldF
  :: ORD
  IMPLIES Word -> Word -> Forest KEY_PARAM a -> Forest KEY_PARAM a -> Forest KEY_PARAM a
meldF !o n !fs !rs
  | n <= 0    = fs
  | otherwise =
      let !o' = unsafeShiftR o 1
          !n' = unsafeShiftR n 1

      in case o .&. 1 of
           0 -> case n .&. 1 of
                  0 -> meldF o' n' fs rs
                  _ -> case rs of
                         Nil             -> Nil -- impossible
                         Tree kb b fb rr ->
                           Tree kb b fb $ meldF o' n' fs rr

           _ -> case fs of
                  Nil             -> Nil -- impossible
                  Tree ka a fa ff ->
                    case n .&. 1 of
                       0 -> Tree ka a fa $ meldF o' n' ff rs
                       _ -> case rs of
                              Nil             -> Nil -- impossible
                              Tree kb b fb rr ->
                                let !(# km, m, fm #) =
                                      if kb < ka
                                        then (# kb, b, Tree ka a fa fb #)
                                        else (# ka, a, Tree kb b fb fa #)

                                in meldCarryF o' n' km m fm ff rr


meldCarryF
  :: ORD
  IMPLIES Word -> Word
  -> KEY -> a -> Forest KEY_PARAM a
  -> Forest KEY_PARAM a -> Forest KEY_PARAM a -> Forest KEY_PARAM a
meldCarryF !o n !kc c !fc !fs !rs
  | n <= 0    = insertF o kc c fc fs
  | otherwise =
      let !o' = unsafeShiftR o 1
          !n' = unsafeShiftR n 1

      in case o .&. 1 of
           0 -> case n .&. 1 of
                  0 -> Tree kc c fc $ meldF o' n' fs rs
                  _ -> case rs of
                         Nil             -> Nil -- impossible
                         Tree kb b fb rr ->
                           let !(# km, m, fm #) =
                                 if kb < kc
                                   then (# kb, b, Tree kc c fc fb #)
                                   else (# kc, c, Tree kb b fb fc #)

                           in meldCarryF o' n' km m fm fs rr

           _ -> case fs of
                  Nil             -> Nil -- impossible
                  Tree ka a fa ff ->
                    case n .&. 1 of
                      0 -> let !(# km, m, fm #) =
                                 if ka < kc
                                   then (# ka, a, Tree kc c fc fa #)
                                   else (# kc, c, Tree ka a fa fc #)

                           in meldCarryF o' n' km m fm ff rs

                      _ -> case rs of
                              Nil             -> Nil -- impossible
                              Tree kb b fb rr ->
                                Tree kc c fc $
                                  let !(# km, m, fm #) =
                                        if kb < ka
                                          then (# kb, b, Tree ka a fa fb #)
                                          else (# ka, a, Tree kb b fb fa #)

                                  in meldCarryF o' n' km m fm ff rr



-- |  \(\mathcal{O}(1)\). Look up a value at the smallest key in the heap.
lookupMin :: Heap KEY_PARAM a -> Maybe a
lookupMin None           = Nothing
lookupMin (Heap _ _ a _) = Just a

-- |  \(\mathcal{O}(1)\). Look up a value at the smallest key in the heap.
lookupMinWithKey :: Heap KEY_PARAM a -> Maybe (Lookup KEY_PARAM a)
lookupMinWithKey None            = Nothing
lookupMinWithKey (Heap _ ka a _) = Just $! Lookup ka a



-- | \(\mathcal{O}(1)\). Apply a function to a value in the heap at the smallest key.
adjustMin :: (a -> a) -> Heap KEY_PARAM a -> Heap KEY_PARAM a
adjustMin _ None            = None
adjustMin f (Heap n ka a t) = Heap n ka (f a) t

-- | \(\mathcal{O}(1)\). Apply a function to a value in the heap at the smallest key.
--
--   New value is evaluated to WHNF.
adjustMin'
  :: (a -> a) -> Heap KEY_PARAM a -> Heap KEY_PARAM a
adjustMin' _ None            = None
adjustMin' f (Heap n ka a t) =
  let !b = f a
  in Heap n ka b t


-- | \(\mathcal{O}(1)\). Apply a function to a value in the heap at the smallest key.
adjustMinWithKey :: (KEY -> a -> a) -> Heap KEY_PARAM a -> Heap KEY_PARAM a
adjustMinWithKey _ None            = None
adjustMinWithKey f (Heap n ka a t) = Heap n ka (f ka a) t

-- | \(\mathcal{O}(1)\). Apply a function to a value in the heap at the smallest key.
--
--   New value is evaluated to WHNF.
adjustMinWithKey'
  :: (KEY -> a -> a) -> Heap KEY_PARAM a -> Heap KEY_PARAM a
adjustMinWithKey' _ None            = None
adjustMinWithKey' f (Heap n ka a t) =
  let !b = f ka a
  in Heap n ka b t



-- | \(\mathcal{O}(\log n)\). Update or delete a value in the heap at the smallest key.
--
--   The 'Maybe' is evaluated to WHNF.
updateMin :: ORD IMPLIES (a -> Maybe a) -> Heap KEY_PARAM a -> Heap KEY_PARAM a
updateMin _ None            = None
updateMin f (Heap n ka a t) =
  case f a of
    Nothing -> deleteMin1 n t
    Just b  -> Heap n ka b t

-- | \(\mathcal{O}(\log n)\). Update or delete a value in the heap at the smallest key.
--
--   The 'Maybe' is evaluated to WHNF.
updateMinWithKey
  :: ORD IMPLIES (KEY -> a -> Maybe a) -> Heap KEY_PARAM a -> Heap KEY_PARAM a
updateMinWithKey _ None            = None
updateMinWithKey f (Heap n ka a t) =
  case f ka a of
    Nothing -> deleteMin1 n t
    Just b  -> Heap n ka b t



-- | \(\mathcal{O}(1)\). Check if the heap is empty.
null :: Heap KEY_PARAM a -> Bool
null None = True
null _    = False

-- | \(\mathcal{O}(1)\). Calculate the number of entries stored in the heap.
size :: Heap KEY_PARAM a -> Word
size None           = 0
size (Heap n _ _ _) = n + 1



-- | \(\mathcal{O}(n)\). Apply a function to every value in the heap.
map :: (a -> b) -> Heap KEY_PARAM a -> Heap KEY_PARAM b
map _ None            = None
map f (Heap n km m t) = Heap n km (f m) (go t)
  where
    go Nil               = Nil
    go (Tree ka a fa ff) = Tree ka (f a) (go fa) (go ff)

-- | \(\mathcal{O}(n)\). Apply a function to every value in the heap.
--
--   New values are evaluated to WHNF.
map'
  :: (a -> b) -> Heap KEY_PARAM a -> Heap KEY_PARAM b
map' _ None            = None
map' f (Heap n km m t) =
  let !b = f m
  in Heap n km b (go t)
  where
    go Nil                  = Nil
    go (Tree ka a fa ff) =
      let !b = f a
      in Tree ka b (go fa) (go ff)

-- | \(\mathcal{O}(n)\). Apply a function to every value in the heap.
mapWithKey :: (KEY -> a -> b) -> Heap KEY_PARAM a -> Heap KEY_PARAM b
mapWithKey _ None            = None
mapWithKey f (Heap n km m t) = Heap n km (f km m) (go t)
  where
    go Nil               = Nil
    go (Tree ka a fa ff) = Tree ka (f ka a) (go fa) (go ff)

-- | \(\mathcal{O}(n)\). Apply a function to every value in the heap.
--
--   New values are evaluated to WHNF.
mapWithKey'
  :: (KEY -> a -> b) -> Heap KEY_PARAM a -> Heap KEY_PARAM b
mapWithKey' _ None            = None
mapWithKey' f (Heap n km m t) =
  let !b = f km m
  in Heap n km b (go t)
  where
    go Nil                  = Nil
    go (Tree ka a fa ff) =
      let !b = f ka a
      in Tree ka b (go fa) (go ff)



-- | \(\mathcal{O}(n_R)\). Fold the heap left-to-right.
foldl :: (b -> a -> b) -> b -> Heap KEY_PARAM a -> b
foldl _ z0 None           = z0
foldl f z0 (Heap _ _ m t) = go (f z0 m) t
  where
    go z Nil              = z
    go z (Tree _ a fa ff) = go (go (f z a) fa) ff

-- | \(\mathcal{O}(n)\). Fold the heap left-to-right with a strict accumulator.
foldl'
  :: (b -> a -> b) -> b -> Heap KEY_PARAM a -> b
foldl' _ z0 None           = z0
foldl' f z0 (Heap _ _ m t) = go (f z0 m) t
  where
    go !z Nil              = z
    go  z (Tree _ a fa ff) =
      let !b = f z a
          !fb = go b fa

      in go fb ff

-- | \(\mathcal{O}(n_R)\). Fold the heap left-to-right.
foldlWithKey :: (b -> KEY -> a -> b) -> b -> Heap KEY_PARAM a -> b
foldlWithKey _ z0 None            = z0
foldlWithKey f z0 (Heap _ km m t) = go (f z0 km m) t
  where
    go z Nil               = z
    go z (Tree ka a fa ff) = go (go (f z ka a) fa) ff

-- | \(\mathcal{O}(n)\). Fold the heap left-to-right with a strict accumulator.
foldlWithKey'
  :: (b -> KEY -> a -> b) -> b -> Heap KEY_PARAM a -> b
foldlWithKey' _ z0 None            = z0
foldlWithKey' f z0 (Heap _ km m t) = go (f z0 km m) t
  where
    go !z Nil                 = z
    go  z (Tree ka a fa ff) =
      let !b = f z ka a
          !fb = go b fa

      in go fb ff



-- | \(\mathcal{O}(n_L)\). Fold the heap right-to-left.
foldr :: (a -> b -> b) -> b -> Heap KEY_PARAM a -> b
foldr _ z0 None           = z0
foldr f z0 (Heap _ _ m t) = f m $ go z0 t
  where
    go z Nil              = z
    go z (Tree _ a fa ff) = f a $ go (go z ff) fa

-- | \(\mathcal{O}(n)\). Fold the heap right-to-left with a strict accumulator.
foldr'
  :: (a -> b -> b) -> b -> Heap KEY_PARAM a -> b
foldr' _ !z0 None           = z0
foldr' f  z0 (Heap _ _ m t) = f m $! go z0 t
  where
    go !z Nil              = z
    go  z (Tree _ a fa ff) =
      let !z1 = go z  ff
          !z2 = go z1 fa

      in f a z2

-- | \(\mathcal{O}(n_L)\). Fold the heap right-to-left.
foldrWithKey :: (KEY -> a -> b -> b) -> b -> Heap KEY_PARAM a -> b
foldrWithKey _ z0 None            = z0
foldrWithKey f z0 (Heap _ km m t) = f km m $ go z0 t
  where
    go z Nil               = z
    go z (Tree ka a fa ff) = f ka a $ go (go z ff) fa

-- | \(\mathcal{O}(n)\). Fold the heap right-to-left with a strict accumulator.
foldrWithKey'
  :: (KEY -> a -> b -> b) -> b -> Heap KEY_PARAM a -> b
foldrWithKey' _ !z0 None            = z0
foldrWithKey' f  z0 (Heap _ km m t) = f km m $! go z0 t
  where
    go !z Nil               = z
    go  z (Tree ka a fa ff) =
      let !z1 = go z  ff
          !z2 = go z1 fa

      in f ka a z2



-- | \(\mathcal{O}(n_M)\).
--   Map each value in a heap to a monoid and combine the results.
foldMap :: Monoid m => (a -> m) -> Heap KEY_PARAM a -> m
foldMap _ None           = mempty
foldMap f (Heap _ _ m t) = f m <> go t
  where
    go Nil              = mempty
    go (Tree _ a fa ff) = f a <> go fa <> go ff

-- | \(\mathcal{O}(n_M)\).
--   Map each value in a heap to a monoid and combine the results.
foldMapWithKey :: Monoid m => (KEY -> a -> m) -> Heap KEY_PARAM a -> m
foldMapWithKey _ None            = mempty
foldMapWithKey f (Heap _ km m t) = f km m <> go t
  where
    go Nil               = mempty
    go (Tree ka a fa ff) = f ka a <> go fa <> go ff



-- | \(\mathcal{O}(n)\).
--   Map each value in a heap to an action, evaluate these actions left-to-right
--   and collect the results.
traverse :: Applicative f => (a -> f b) -> Heap KEY_PARAM a -> f (Heap KEY_PARAM b)
traverse _ None            = pure None
traverse f (Heap n km m t) = liftA2 (Heap n km) (f m) (go t)
  where
    go Nil               = pure Nil
    go (Tree ka a fa ff) = liftA3 (Tree ka) (f a) (go fa) (go ff)

-- | \(\mathcal{O}(n)\).
--   Map each value in a heap to an action, evaluate these actions left-to-right
--   and collect the results.
traverseWithKey
  :: Applicative f => (KEY -> a -> f b) -> Heap KEY_PARAM a -> f (Heap KEY_PARAM b)
traverseWithKey _ None            = pure None
traverseWithKey f (Heap n km m t) = liftA2 (Heap n km) (f km m) (go t)
  where
    go Nil               = pure Nil
    go (Tree ka a fa ff) = liftA3 (Tree ka) (f ka a) (go fa) (go ff)
