Flabbergast: Variations on a Theme
==================================

This is a collection of algorithms for automatically playing a word game called
Flabbergast. (Flabbergast is similar to [Boggle], but with key differences.
I've renamed it out of respect for any possible trademarks[^1].)

I started with a fairly naive approach, and incrementally improved things by
applying both original ideas, and suggestions from the internet. The algorithms
range in performance (on a 4x4 board) from ridiculously slow to competitive with
`cat`.

All the algorithms are expressed in Haskell.

The Game
--------

In Flabbergast, the player is faced with a grid of letters, and charged with
finding all possible words that can be assembled from the letters within
certain rules:

- Each grid position can only be used once in each word (i.e. if the word
  contains the substring "EE", you must find two separate E locations).
  Separate words can all use the same position, however.

- Each successive letter must be one hop away from the previous, including
  diagonals.

- Words must be found in the dictionary, provided as a sorted list of words in
  text format.

- The player must record, not only the words found, but the coordinates of each
  letter that was used. This simplifies the judge's job. Coordinates are
  Cartesian `(x, y)`, where the upper-left corner of the board is `(0, 0)`.

Any word in the dictionary is fair game; there is no upper or lower limit on
word length. Each word found (and backed up with coordinates) is worth one
point.

There is no "Qu" space as in Boggle. Instead, all QU pairs in the dictionary
are replaced by just Q.

For the purposes of this implementation, letters in the board and dictionary
are restricted to 8-bit ISO-8859-1.

The Algorithms
--------------

Conceptually, a Flabbergast-playing algorithm is one that takes a dictionary
file and a grid of letters, and produces a list of (word, coordinate list)
pairs. In Haskell terms,

```haskell
Dictionary -> Board -> [(Word, [Pos])]
```

Arbitrary pre-processing of the dictionary is okay, so long as it's done without
reference to the board. In practice, the most common preprocessing requirement
was access to sorted versions of the letters in each word, so I've just encoded
that in the dictionary file.

The algorithms fall into two broad categories:

- **Traversals.** These explore potential paths in the board, checking against
  the dictionary as needed. Most of the interesting tricks here involve pruning
  potential paths or speeding up dictionary checks. Typically dominated by
  `O(n*m)` for an *n* x *m* board, but there are some surprises.

- **Dynamic Programming.** These compute multiple potential paths
  simultaneously, and operate on a word-by-word basis. Improvements here involve
  how the path computation proceeds and which words are considered. Typically
  dominated by `O(d)` for a *d*-word dictionary.

Leaderboard for 4x4 boards last time I updated this README (fastest to slowest
on my Haswell machine):

1. 5.92ms/4x4: [Traversal.NotHeap](../tree/master/src/Traversal/NotHeap.hs)
1. 5.96ms/4x4: [Traversal.Heap](../tree/master/src/Traversal/Heap.hs)
1. 19.5ms/4x4: [Traversal.FilteredTrie](../tree/master/src/Traversal/FilteredTrie.hs)
1. 20.3ms/4x4: [DP.FilteredOnePassTree](../tree/master/src/DP/FilteredOnePassTree.hs)
1. 20.9ms/4x4: [DP.FilteredOnePass](../tree/master/src/DP/FilteredOnePass.hs)
1. 21.6ms/4x4: [Traversal.IncrementalFPHAMT](../tree/master/src/Traversal/IncrementalFPHAMT.hs)
1. 24.4ms/4x4: [Traversal.FilteredPrefixHAMT](../tree/master/src/Traversal/FilteredPrefixHAMT.hs)
1. 25.6ms/4x4: [Traversal.FilteredPrefixSet](../tree/master/src/Traversal/FilteredPrefixSet.hs)
1. 222ms/4x4: [DP.TwoPass](../tree/master/src/DP/TwoPass.hs)
1. 231ms/4x4: [DP.OnePass](../tree/master/src/DP/OnePass.hs)
1. 305ms/4x4: [Traversal.Trie](../tree/master/src/Traversal/Trie.hs)

And among the algorithms that are too slow to complete a 4x4 board before I get
bored, here are the times for a 2x2 board:

1. 10.5ms/2x2: [Traversal.FilteredHAMT](../tree/master/src/Traversal/FilteredHAMT.hs)
1. 11.0ms/2x2: [Traversal.FilteredSet](../tree/master/src/Traversal/FilteredSet.hs)
1. 42.1ms/2x2: [Traversal.Set](../tree/master/src/Traversal/Set.hs)
1. 187ms/2x2: [Traversal.List](../tree/master/src/Traversal/List.hs)

And memory allocations per 2x2 board (note, this is *allocated*, not live --
most gets garbage collected immediately).

| Case                         |     Allocated |   GCs |
| ---------------------------- | ------------: | ----: |
| Traversal.NotHeap            |     3,336,832 |     3 |
| Traversal.FilteredHeap       |     3,671,104 |     3 |
| Traversal.FilteredTrie       |     3,790,416 |     3 |
| Traversal.IncrementalFPHAMT  |     4,391,072 |     4 |
| Traversal.Heap               |     4,945,728 |     4 |
| Traversal.FilteredPrefixHAMT |     8,966,888 |     6 |
| Traversal.FilteredPrefixSet  |    11,884,288 |    11 |
| DP.FilteredOnePassTree       |    14,402,224 |    13 |
| DP.FilteredOnePass           |    17,289,736 |    16 |
| Traversal.Trie               |   332,071,664 |   268 |
| DP.TwoPass                   |   911,207,128 |   881 |
| DP.OnePass                   | 1,062,036,680 | 1,026 |
| Traversal.Set                | 4,162,694,960 | 4,008 |
| Traversal.FilteredSet        | 4,133,006,656 | 3,979 |
| Traversal.FilteredHAMT       | 4,326,976,040 | 4,178 |

The main lesson there, in my opinion, is that some algorithms that would win
from a complexity-analysis perspective (like `Traversal.Trie`, or `DP.OnePass`
compared to `TwoPass`) lose in practice because of memory allocation patterns.

[^1]: I haven't actually checked whether Boggle is currently trademarked;
      coding is much more fun than searching the USPTO database.

[Boggle]: https://en.wikipedia.org/wiki/Boggle
