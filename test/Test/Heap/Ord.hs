{-# LANGUAGE StandaloneDeriving
           , TypeApplications #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Heap.Ord
  ( heap
  ) where

import           Data.Heap.Ord (Heap (..), Lookup (..))
import qualified Data.Heap.Ord as Heap
import           Data.Heap.Ord.Debug (Validity (..), Reason (..))
import qualified Data.Heap.Ord.Debug as Heap

import           Data.IORef
import qualified Data.List as List
import           Data.Monoid
import           Test.Hspec



deriving instance Eq k => Eq (Validity k)
deriving instance Eq k => Eq (Reason k)



fromList :: Ord k => [(k, a)] -> Heap k a
fromList = foldl' (\z (k, a) -> Heap.insert k a z) Empty

toListMin :: Ord k => Heap k a -> [(k, a)]
toListMin Empty              = []
toListMin (Lookup k a :<| h) = (k, a) : toListMin h



heap :: Spec
heap = do
  it "empty" $
    toListMin (Empty @Int @Int) `shouldBe` []

  it "singleton" $
    toListMin (Heap.singleton @Int 1 'a') `shouldBe` [(1, 'a')]

  describe "insert/size/viewMin" $ do
    it "Ascending" $ do
      let ref = fmap (\x -> (x, negate x)) [1..256]

          h = fromList @Int @Int ref

      (Heap.validate h, Heap.size h, toListMin h)
        `shouldBe` (Valid, 256, ref)

    it "Descending" $ do
      let ref = fmap (\x -> (x, negate x)) [1024,1023..1]

          h = fromList @Int ref

      (Heap.validate h, Heap.size h, toListMin h)
        `shouldBe` (Valid, 1024, List.sortOn fst ref)

    it "Zipped" $ do
      let ref = mconcat $ zipWith (\a b -> [a,b])
                            (fmap (\x -> (x, negate x)) [1,3..4095])
                            (fmap (\x -> (x, negate x)) [4096,4094..2])

          h = fromList @Int ref

      (Heap.validate h, Heap.size h, toListMin h)
        `shouldBe` (Valid, 4096, List.sortOn fst ref)

  describe "union/size/viewMin" $ do
    it "Empty/_" $ do
      let refA = fmap (\x -> (x, negate x)) [1..256]

          hA = fromList @Int refA
          hC = Heap.union hA Empty

      hA `shouldBe` hC

    it "_/Empty" $ do
      let refB = fmap (\x -> (x, negate x)) [1..256]

          hB = fromList @Int refB
          hC = Heap.union Empty hB

      hB `shouldBe` hC

    it "3860/2313" $ do
      let refA = fmap (\x -> (x, negate x)) [1,3..7719]
          refB = fmap (\x -> (x, negate x)) [4,8..9252]

          hC = Heap.union (fromList @Int refA) (fromList @Int refB)

      (Heap.validate hC, Heap.size hC, toListMin hC)
        `shouldBe` (Valid, 6173, List.sortOn fst $ refA <> refB)

    it "2930/3795" $ do
      let refA = fmap (\x -> (x, negate x)) [5860,5858..2]
          refB = fmap (\x -> (x, negate x)) [1,3..7590]

          hC = Heap.union (fromList @Int refA) (fromList @Int refB)

      (Heap.validate hC, Heap.size hC, toListMin hC)
        `shouldBe` (Valid, 6725, List.sortOn fst $ refA <> refB)

    it "777/3281" $ do
      let refA = fmap (\x -> (x, negate x)) [1,3..1553]
          refB = fmap (\x -> (x, negate x)) [6562,6560..2]

          hC = Heap.union (fromList @Int refA) (fromList @Int refB)

      (Heap.validate hC, Heap.size hC, toListMin hC)
        `shouldBe` (Valid, 4058, List.sortOn fst $ refA <> refB)

    it "256/4096" $ do
      let refA = fmap (\x -> (x, negate x)) [1,3..511]
          refB = fmap (\x -> (x, negate x)) [8192,8190..2]

          hC = Heap.union (fromList @Int refA) (fromList @Int refB)

      (Heap.validate hC, Heap.size hC, toListMin hC)
        `shouldBe` (Valid, 4352, List.sortOn fst $ refA <> refB)

  describe "null" $ do
    it "empty" $
      null Empty `shouldBe` True

    it "singleton" $
      null (Heap.singleton @Int 1 'e') `shouldBe` False

  describe "min" $ do
    let ref  = fmap (\x -> (x, negate x)) $ [1023,1021..1] <> [2,4..1024]
        ref' = fmap (\x -> (x, negate x)) $ [1023,1021..3] <> [2,4..1024]
        h = fromList ref

    it "lookupMin" $
      Heap.lookupMin @Int h `shouldBe` Just (-1)

    it "lookupMinWithKey" $ do
      Heap.lookupMinWithKey @Int h `shouldBe` Just (Lookup 1 (-1))

    it "adjustMin" $
      toListMin (Heap.adjustMin @Int negate h) `shouldBe` (1,1) : List.sortOn fst ref'

    it "adjustMin'" $
      toListMin (Heap.adjustMin' @Int negate h) `shouldBe` (1,1) : List.sortOn fst ref'

    it "adjustMinWithKey" $
      toListMin (Heap.adjustMinWithKey @Int (+) h) `shouldBe` (1,0) : List.sortOn fst ref'

    it "adjustMin'" $
      toListMin (Heap.adjustMinWithKey' @Int (+) h) `shouldBe` (1,0) : List.sortOn fst ref'

    it "deleteMin" $
      toListMin (Heap.deleteMin h) `shouldBe` List.sortOn fst ref'

    describe "updateMin" $ do
      it "Just" $ do
        toListMin (Heap.updateMin @Int (Just . negate) h)
          `shouldBe` (1,1) : List.sortOn fst ref'

      it "Nothing" $ do
        toListMin (Heap.updateMin @Int (\_ -> Nothing) h)
          `shouldBe` List.sortOn fst ref'

    describe "updateMinWithKey" $ do
      it "Just" $ do
        toListMin (Heap.updateMinWithKey @Int (\k a -> Just $ k + a) h)
          `shouldBe` (1,0) : List.sortOn fst ref'

      it "Nothing" $ do
        toListMin (Heap.updateMinWithKey @Int (\_ _ -> Nothing) h)
          `shouldBe` List.sortOn fst ref'

  describe "map" $ do
    let ref = fmap (\x -> (x, negate x)) [1..1024]
    it "map" $ do
      toListMin @Int (Heap.map negate $ fromList ref)
        `shouldBe` map (\(k, a) -> (k, negate a)) ref

    it "map'" $ do
      toListMin @Int (Heap.map' negate $ fromList ref)
        `shouldBe` map (\(k, a) -> (k, negate a)) ref

    it "mapWithKey" $ do
      toListMin @Int (Heap.mapWithKey (+) $ fromList ref)
        `shouldBe` map (\(k, a) -> (k, k + a)) ref

    it "mapWithKey'" $ do
      toListMin @Int (Heap.mapWithKey' (+) $ fromList ref)
        `shouldBe` map (\(k, a) -> (k, k + a)) ref

  describe "fold" $ do
    let ref = fmap (\x -> (x, negate x)) [1..1024 :: Int]
     
        h = fromList ref

    it "foldl-foldr" $
      Heap.foldl (flip (:)) [] h `shouldBe` List.reverse (Heap.foldr (:) [] h)

    it "foldl-foldl'" $
      Heap.foldl' (flip (:)) [] h `shouldBe` Heap.foldl (flip (:)) [] h

    it "foldr-foldMap" $
      Heap.foldr (:) [] h `shouldBe` Heap.foldMap pure h

    it "foldr-foldr'" $
      Heap.foldr' (:) [] h `shouldBe` Heap.foldr (:) [] h
    
    it "foldMap-lookupMin" $
      getFirst (Heap.foldMap (First . Just) h) `shouldBe` Heap.lookupMin h

    it "foldlWithKey-foldrWithKey" $
      Heap.foldlWithKey (\z k a -> (k, a):z) [] h
        `shouldBe` List.reverse (Heap.foldrWithKey (\k a -> (:) (k, a)) [] h)

    it "foldlWithKey-foldlWithKey'" $
      Heap.foldlWithKey' (\z k a -> (k, a):z) [] h
        `shouldBe` Heap.foldlWithKey (\z k a -> (k, a):z) [] h

    it "foldrWithKey-foldMapWithKey" $
      Heap.foldrWithKey (\k a -> (:) (k, a)) [] h
        `shouldBe` Heap.foldMapWithKey (\k a -> [(k, a)]) h

    it "foldrWithKey-foldrWithKey'" $
      Heap.foldrWithKey' (\k a -> (:) (k, a)) [] h
        `shouldBe` Heap.foldrWithKey (\k a -> (:) (k, a)) [] h

    it "foldMapWithKey-lookupMinWithKey" $
      getFirst (Heap.foldMapWithKey (\k a -> First . Just $ Lookup k a) h)
        `shouldBe` Heap.lookupMinWithKey h

  describe "traverse" $ do
    let ref = fmap (\x -> (x, negate x)) [1..1024]
    it "traverse" $ do
      var <- newIORef 0
      ref' <- Heap.traverse @_ @Int
                (\a -> do modifyIORef' var (+ a)
                          pure $ negate a
                )
                $ fromList ref

      s <- readIORef var

      (s, toListMin ref')
        `shouldBe` (sum $ fmap snd ref, fmap (\(k, a) -> (k, negate a)) ref)

    it "traverseWithKey" $ do
      var <- newIORef 0
      ref' <- Heap.traverseWithKey @_ @Int
                (\k a -> do let b = k + a
                            modifyIORef' var (+ b)
                            pure b
                )
                $ fromList ref

      s <- readIORef var

      (s, toListMin ref')
        `shouldBe` (sum $ fmap (\(k, a) -> k + a) ref, fmap (\(k, a) -> (k, k + a)) ref)
