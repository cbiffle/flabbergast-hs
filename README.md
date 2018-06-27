Flabbergast: Variations on a Theme
==================================

This is a collection of algorithms for automatically playing a word game called
Flabbergast. (Flabbergast is similar to [Boggle], but with key differences.
I've renamed it out of respect for any possible trademarks.)

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

## Timing

Leaderboard for 6x6, 4x4, and 2x2 board sizes last time I updated this README:

(Measured on a lightly-loaded Intel Haswell i5-4200U. Blank cells are cases
where the algorithm is slow enough that I get bored.)

| Case                           | 6x6 ms | 4x4 ms | 2x2 ms |
| ------------------------------ | -----: | -----: | -----: |
| [Traversal.NotHeap]            | 10.2   | 4.58   | 3.03   |
| [Traversal.Heap]               | 12.8   | 5.13   | 2.91   |
| [Traversal.FilteredHeap]       | 44.4   | 17.7   | 10.6   |
| [Traversal.FilteredTrie]       | 59.0   | 19.5   | 9.88   |
| [DP.FilteredOnePassTree]       | 93.7   | 20.3   | 11.4   |
| [DP.FilteredOnePass]           | 99.3   | 20.9   | 9.99   |
| [Traversal.IncrementalFPHAMT]  | 73.4   | 21.6   | 10.2   |
| [Traversal.FilteredPrefixHAMT] | 120    | 24.4   | 9.85   |
| [Traversal.FilteredPrefixSet]  | 117    | 25.6   | 11.3   |
| [Traversal.Trie]               | 282    | 278    | 280    |
| [DP.TwoPass]                   | 492    | 222    | 47.9   |
| [DP.OnePass]                   | 511    | 231    | 44.5   |
| [Traversal.FilteredHAMT]       |        | 7214   | 11.4   |
| [Traversal.FilteredSet]        |        | 8626   | 9.99   |
| [Traversal.Set]                |        | 10580  | 41.7   |
| [Traversal.List]               |        |        | 201    |

Observations:

- [Traversal.Heap] and [Traversal.NotHeap], which use a [clever algorithm
  described by M.J. Hecht](http://www.mh-z.com/untangle/alg_heap.html), are
  consistently the fastest. [Traversal.NotHeap] tends win between the two, which
  shows that the performance advantage in Hecht's algorithm is not the use of
  the heap data structure, but the linear dictionary traversal.

- Using a naive [trie] makes traversals very fast, but runtime is dominated by
  the cost of constructing said trie. Granted, the trie structure I'm currently
  using is not very compact.

- Filtering the dictionary to words that could be constructed out of the letters
  available in the board is a huge improvement to algorithms that build
  expensive dictionary-based datastructures (e.g. [Traversal.FilteredTrie] vs
  [Traversal.Trie]) and algorithms that require a full pass over the dictionary
  (e.g. [DP.FilteredOnePass] vs [DP.OnePass]). It actually *hurts* the
  performance of Hecht traversals like [Traversal.Heap], as you can see by
  comparing with [Traversal.FilteredHeap].

- If you have a set-like data structure that is primarily used for membership
  tests (and you have sane hashcode implementations for your types), a [HAMT] 
  wins out over size-balanced binary trees. This is what you'd expect from the
  analysis of each structure, but it's nice to see it check out.

[HAMT]: https://en.wikipedia.org/wiki/Hash_array_mapped_trie
[trie]: https://en.wikipedia.org/wiki/Trie

## Memory allocation

This table gives total bytes allocated while solving a particular 2x2 board.
Note that this measures *allocations made*, not how much memory was in use at a
given time -- most of the bytes allocated get GC'd immediately. This serves as a
useful estimate of allocator/GC overhead.

| Case                         |     Allocated |   GCs |
| ---------------------------- | ------------: | ----: |
| Traversal.NotHeap            |     2,348,760 |     2 |
| Traversal.FilteredHeap       |     3,671,080 |     3 |
| Traversal.FilteredTrie       |     3,778,984 |     3 |
| Traversal.Heap               |     4,336,208 |     4 |
| Traversal.IncrementalFPHAMT  |     4,391,224 |     4 |
| Traversal.FilteredPrefixHAMT |     9,254,624 |     7 |
| Traversal.FilteredPrefixSet  |    12,222,256 |    11 |
| DP.FilteredOnePassTree       |    14,401,832 |    13 |
| Traversal.List               |    15,767,216 |    15 |
| DP.FilteredOnePass           |    17,289,544 |    16 |
| Traversal.Trie               |   370,129,536 |   357 |
| DP.TwoPass                   |   911,207,128 |   881 |
| DP.OnePass                   | 1,062,036,680 | 1,026 |
| Traversal.FilteredSet        | 6,300,293,280 | 6,083 |
| Traversal.Set                | 6,329,967,048 | 6,112 |
| Traversal.FilteredHAMT       | 6,494,262,872 | 6,266 |

The main lesson there, in my opinion, is that some algorithms that would win
from a complexity-analysis perspective (like `Traversal.Trie`, or `DP.OnePass`
compared to `TwoPass`) lose in practice because of memory allocation patterns.


[Boggle]: https://en.wikipedia.org/wiki/Boggle

[DP.FilteredOnePass]: src/DP/FilteredOnePass.hs
[DP.FilteredOnePassTree]: src/DP/FilteredOnePassTree.hs
[DP.OnePass]: src/DP/OnePass.hs
[DP.TwoPass]: src/DP/TwoPass.hs
[Traversal.FilteredHAMT]: src/Traversal/FilteredHAMT.hs
[Traversal.FilteredHeap]: src/Traversal/FilteredHeap.hs
[Traversal.FilteredPrefixHAMT]: src/Traversal/FilteredPrefixHAMT.hs
[Traversal.FilteredPrefixSet]: src/Traversal/FilteredPrefixSet.hs
[Traversal.FilteredSet]: src/Traversal/FilteredSet.hs
[Traversal.FilteredTrie]: src/Traversal/FilteredTrie.hs
[Traversal.Heap]: src/Traversal/Heap.hs
[Traversal.IncrementalFPHAMT]: src/Traversal/IncrementalFPHAMT.hs
[Traversal.List]: src/Traversal/List.hs
[Traversal.NotHeap]: src/Traversal/NotHeap.hs
[Traversal.Set]: src/Traversal/Set.hs
[Traversal.Trie]: src/Traversal/Trie.hs
