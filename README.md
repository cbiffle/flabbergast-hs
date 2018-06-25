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

[^1]: I haven't actually checked whether Boggle is currently trademarked;
      coding is much more fun than searching the USPTO database.

[Boggle]: https://en.wikipedia.org/wiki/Boggle
