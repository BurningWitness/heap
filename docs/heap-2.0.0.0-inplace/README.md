# heap [![Hackage](https://img.shields.io/hackage/v/heap.svg)](https://hackage.haskell.org/package/heap)

A Haskell library for [binominal heaps](https://en.wikipedia.org/wiki/Binomial_heap).

Featuring:

- `Data.Heap.Ord`: spine-strict binomial min-heap with a global root over any key
  type that has an `Ord` instance.

  This is roughly on par with
  [`pqueue#MinPQueue`](https://hackage.haskell.org/package/pqueue-1.5.0.0/docs/Data-PQueue-Prio-Min.html#t:MinPQueue) in both time and space overhead, though `MinPQueue` keeps some
  of the spine lazy for amortization.

  [`heaps#Heap`](https://hackage.haskell.org/package/heaps-0.4/docs/Data-Heap.html#t:Heap)
  is a variant of a binomial heap that uses skew binary numbers to achieve far better
  meld efficiency at the cost of entry lookup performance.

- `Data.Heap.Float`,``Data.Heap.Double` and `Data.Heap.Word` are copies of
  `Data.Heap.Ord` specialized for their respective data types.

  Benchmarks show these are roughly twice as fast as their generic counterparts
  for small sample sizes, on top of taking up less space per entry.
