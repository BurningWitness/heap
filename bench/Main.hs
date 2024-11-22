{-# LANGUAGE BangPatterns
           , FlexibleContexts
           , PackageImports
           , NumericUnderscores #-}

module Main where

import qualified "heap"  Data.Heap    as Heap
import qualified "heaps" Data.Heap    as Heaps
import qualified Data.PQueue.Prio.Min as PQueue

import qualified Data.List as List
import           Data.Ord
import           System.Random
import           Test.Tasty.Bench



generate :: RandomGen g => Int -> g -> [(Word, Int)]
generate len = go 0
  where
    go n g
      | n >= len = []
      | otherwise      =
          let (!k, g') = uniformR (1, max 2 $ fromIntegral len) g
          in (k, n) : go (n + 1) g'



smallN, mediumN, largeN :: Int
smallN  = 256
mediumN = 16384
largeN  = 262144

small, medium, large :: [(Word, Int)]
small  = generate smallN  (mkStdGen 0)
medium = generate mediumN (mkStdGen 1)
large  = generate largeN  (mkStdGen 2)

sizes :: [[(Int, StdGen)]]
sizes =
  fmap (\(n, m) -> (n, mkStdGen m)) <$>
    [ zip [300,307..405] [0..15]
    , zip [3000,3070..4050] [16..31]
    , zip [30000,30700..40500] [32..48]
    ]

withSizes :: (Int -> [(Word, Int)] -> [Benchmark]) -> [Benchmark]
withSizes f =
  fmap
    ( \xs ->
        case xs of
          (n, g) : _ ->
            bgroup (show n) $
              f n (generate n g)

          _ -> error "Empty size spec"
    )
    sizes

withSizes16x
  :: ([[(Word, Int)]] -> [Benchmark]) -> [Benchmark]
withSizes16x f =
  fmap
    ( \xs ->
        case xs of
          (nA, _) : (nB, _) : _ ->
            bgroup (shows nA . showChar '/' . shows nB $ "/_") $
              f $ fmap (uncurry generate) xs

          _ -> error "Empty size spec"
    )
    sizes

withOrderedSizes :: ([(Word, Int)] -> [Benchmark]) -> [Benchmark]
withOrderedSizes f =
  fmap
    ( \xs ->
        case xs of
          (n, g) : _ ->
            let raw = generate n g
            in bgroup (show n)
                 [ bgroup "ascending"  $ f (List.sortOn fst raw)
                 , bgroup "descending" $ f (List.sortOn (Down . fst) raw)
                 , bgroup "random"     $ f raw
                 ]

          _ -> error "Empty size spec"
    )
    sizes

withWobblingSizes :: ([(Word, Int)] -> Int -> [(Word, Int)] -> [Benchmark]) -> [Benchmark]
withWobblingSizes f =
  fmap
    ( \xs ->
        case xs of
          (n, gA) : (n', gB) : _ ->
            let nA = n `quot` 16
                nB = n' * 4

                rawA = generate nA gA
                rawB = generate nB gB

            in bgroup (shows nB . showString " over " $ show nA)
                 [ bgroup "ascending"  $ f rawA nB (List.sortOn fst rawB)
                 , bgroup "descending" $ f rawA nB (List.sortOn (Down . fst) rawB)
                 , bgroup "random"     $ f rawA nB rawB
                 ]

          _ -> error "Empty size spec"
    )
    sizes



heap_populate :: Heap.MinPrioHeap Word Int -> [(Word, Int)] -> Heap.MinPrioHeap Word Int
heap_populate =
  foldl' (\z (k, a) -> Heap.insert (k, a) z)

heaps_populate
  :: Heaps.Heap (Heaps.Entry Word Int) -> [(Word, Int)] -> Heaps.Heap (Heaps.Entry Word Int)
heaps_populate =
  foldl' (\z (k, a) -> Heaps.insert (Heaps.Entry k a) z)

pqueue_populate :: PQueue.MinPQueue Word Int -> [(Word, Int)] -> PQueue.MinPQueue Word Int
pqueue_populate =
  foldl' (\z (k, a) -> PQueue.insert k a z)



heap_reduce :: Int -> Heap.MinPrioHeap Word Int -> IO (Heap.MinPrioHeap Word Int)
heap_reduce n !h
  | n <= 0    = pure h
  | otherwise =
      case Heap.view h of
        Nothing       -> fail $ "Empty at " <> show n
        Just (_, !h') -> heap_reduce (n - 1) h'

heaps_reduce
  :: Int -> Heaps.Heap (Heaps.Entry Word Int) -> IO (Heaps.Heap (Heaps.Entry Word Int))
heaps_reduce n !h
  | n <= 0    = pure h
  | otherwise =
      case Heaps.uncons h of
        Nothing       -> fail $ "Empty at " <> show n
        Just (_, !h') -> heaps_reduce (n - 1) h'

pqueue_reduce :: Int -> PQueue.MinPQueue Word Int -> IO (PQueue.MinPQueue Word Int)
pqueue_reduce n !h
  | n <= 0    = pure h
  | otherwise =
      case PQueue.minView h of
        Nothing       -> fail $ "Empty at " <> show n
        Just (_, !h') -> pqueue_reduce (n - 1) h'



heap_meld :: [Heap.MinPrioHeap Word Int] -> Heap.MinPrioHeap Word Int
heap_meld    []  = Heap.empty
heap_meld (a:bs) = go a bs
  where
    go !x    []  = x
    go  x (y:zs) = go (Heap.union x y) zs

heaps_meld :: [Heaps.Heap (Heaps.Entry Word Int)] -> Heaps.Heap (Heaps.Entry Word Int)
heaps_meld    []  = Heaps.empty
heaps_meld (a:bs) = go a bs
  where
    go x    []  = x
    go x (y:zs) = go (Heaps.union x y) zs

pqueue_meld :: [PQueue.MinPQueue Word Int] -> PQueue.MinPQueue Word Int
pqueue_meld    []  = PQueue.empty
pqueue_meld (a:bs) = go a bs
  where
    go !x    []  = x
    go  x (y:zs) = go (PQueue.union x y) zs



heap_wobble :: Heap.MinPrioHeap Word Int -> [(Word, Int)] -> IO (Heap.MinPrioHeap Word Int)
heap_wobble = go (0 :: Int)
  where
    go !_ !h           []  = pure h
    go  n  h ((k, a) : xs) =
      let h' = Heap.insert (k, a) h
      in case Heap.view h' of
           Nothing       -> fail $ "Empty at " <> show n
           Just (_, h'') -> go (n + 1) h'' xs

heaps_wobble
  :: Heaps.Heap (Heaps.Entry Word Int) -> [(Word, Int)]
  -> IO (Heaps.Heap (Heaps.Entry Word Int))
heaps_wobble = go (0 :: Int)
  where
    go !_ !h           []  = pure h
    go  n  h ((k, a) : xs) =
      let h' = Heaps.insert (Heaps.Entry k a) h
      in case Heaps.uncons h' of
           Nothing       -> fail $ "Empty at " <> show n
           Just (_, h'') -> go (n + 1) h'' xs

pqueue_wobble
  :: PQueue.MinPQueue Word Int -> [(Word, Int)] -> IO (PQueue.MinPQueue Word Int)
pqueue_wobble = go (0 :: Int)
  where
    go !_ !h           []  = pure h
    go  n  h ((k, a) : xs) =
      let h' = PQueue.insert k a h
      in case h' of
           PQueue.Empty    -> fail $ "Empty at " <> show n
           _ PQueue.:< h'' -> go (n + 1) h'' xs



heap_bigWobble
  :: Int -> Int -> Heap.MinPrioHeap Word Int -> [(Word, Int)]
  -> IO (Heap.MinPrioHeap Word Int)
heap_bigWobble k = go
  where
    go !n !h xs
      | n <= 0    = pure h
      | otherwise = do
          let (ys, zs) = splitAt (min n k) xs
              h' = heap_populate h ys

          h'' <- heap_reduce (min n k) h'
          go (n - k) h'' zs

heaps_bigWobble
  :: Int -> Int -> Heaps.Heap (Heaps.Entry Word Int) -> [(Word, Int)]
  -> IO (Heaps.Heap (Heaps.Entry Word Int))
heaps_bigWobble k = go
  where
    go !n !h xs
      | n <= 0 = pure h
      | otherwise = do
          let (ys, zs) = splitAt (min n k) xs
              h' = heaps_populate h ys

          h'' <- heaps_reduce (min n k) h'
          go (n - k) h'' zs

pqueue_bigWobble
  :: Int -> Int -> PQueue.MinPQueue Word Int -> [(Word, Int)]
  -> IO (PQueue.MinPQueue Word Int)
pqueue_bigWobble k = go
  where
    go !n !h xs
      | n <= 0 = pure h
      | otherwise = do
          let (ys, zs) = splitAt (min n k) xs
              h' = pqueue_populate h ys

          h'' <- pqueue_reduce (min n k) h'
          go (n - k) h'' zs



main :: IO ()
main =
  defaultMain
    [ bgroup "insert" $
        withOrderedSizes $ \load ->
          [ bench "Heap" $
              flip whnf load $ heap_populate Heap.empty
          , bench "Heaps" $
              flip whnf load $ heaps_populate Heaps.empty
          , bench "MinPQueue" $
              flip whnf load $ pqueue_populate PQueue.empty
          ]

    , bgroup "reduce" $
        withSizes $ \n load ->
          [ bench "Heap" $
              flip whnfAppIO (heap_populate Heap.empty load) $
                heap_reduce n

          , bench "Heaps" $
              flip whnfAppIO (heaps_populate Heaps.empty load) $
                heaps_reduce n

          , bench "MinPQueue" $
              flip whnfAppIO (pqueue_populate PQueue.empty load) $
                pqueue_reduce n
          ]

    , bgroup "meld" $
        withSizes16x $ \loads ->
          [ bench "Heap.Ord" $
              flip whnf (heap_populate Heap.empty <$> loads) $
                heap_meld

          , bench "Heaps" $
              flip whnf (heaps_populate Heaps.empty <$> loads) $
                heaps_meld

          , bench "MinPQueue" $
              flip whnf (pqueue_populate PQueue.empty <$> loads) $
                pqueue_meld
          ]

    , bgroup "wobble x1" $
        withWobblingSizes $ \base _ load ->
          [ bench "Heap.Ord" $
              flip whnfAppIO (heap_populate Heap.empty base, load) $
                uncurry heap_wobble

          , bench "Heaps" $
              flip whnfAppIO (heaps_populate Heaps.empty base, load) $
                uncurry heaps_wobble

          , bench "MinPQueue" $
              flip whnfAppIO (pqueue_populate PQueue.empty base, load) $
                uncurry pqueue_wobble
          ]

    , bgroup "wobble x50" $
        withWobblingSizes $ \base n load ->
          [ bench "Heap.Ord" $
              flip whnfAppIO (heap_populate Heap.empty base, load) $
                uncurry (heap_bigWobble 50 n)

          , bench "Heaps" $
              flip whnfAppIO (heaps_populate Heaps.empty base, load) $
                uncurry (heaps_bigWobble 50 n)

          , bench "MinPQueue" $
              flip whnfAppIO (pqueue_populate PQueue.empty base, load) $
                uncurry (pqueue_bigWobble 50 n)
          ]
    ]
