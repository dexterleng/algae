# The MOSS algorithm

## 1. Preprocessing

This step will:

1. remove all whitespaces and newlines
2. replace variable names and language keywords with a common identifier (e.g. "V")


We need to able to map a processed character from its index in the processed file to its index before it was processed. This allows us to display the plagiarised code in the original form instead of the processed one.

```
process("A BC D variable_name")
Output: [(A, (0, 0)), (B, (2, 2)), (C, (3, 3)), (D, (5, 5)), (V, (7, 19))]
type: [(processed_character, (original_starting_index, original_ending_index))]
```

Let's define `(original_starting_index, original_ending_index)` as `OriginalPosition`.

The output of our pre-processing function should be `[(ProcessedCharacter, OriginalPosition)]`.

# 2. Generating hashed "k-grams"

A k-gram is a contiguous sequence of elements (characters) of length `k`.

Here is an example of all possible k-grams from a string:

```
kGrams("ABCD", k=2)
Output: [AB, BC, CD]
```

We have to hash the generated k-grams:

```
hashedKGrams([A, B, C, D], k=2)
Output: [
	H(AB),
	H(BC),
	H(CD),
]
```

Hashing sliding-window sequences like the k-gram can be done in linear time using a [rolling hash](https://en.wikipedia.org/wiki/Rolling_hash).

To map a hash back to the original characters it represents, we need to be able to map a hash to the first and last processed character index (then map that to the original characters):

```
k_grams = hashedKGrams([A, B, C, D], k=2)
F(i) = (i, i + k - 1)
a = map F k_grams // [(FirstProcessedCharacterIndex, LastProcessedCharacterIndex)]
```

We can use the `ProcessedCharacterIndex` to lookup the starting and ending `OriginalPositions`.

Create a `HashMap<Hash, (FirstProcessedCharacterIndex, LastProcessedCharacterIndex)>` for fast lookup time.

# 3. Picking a subset of k-gram hashes (Winnowing)

We have to define a "window" size `w` to form all length `w` contiguous sequences of hashes:

```
windows([H(AB), H(BC), H(CD), H(DE)], w=2)
Output: [
	[H(AB), H(BC)],
	[H(BC), H(CD)],
	[H(CD), H(DE)],
]
```

Each element (list of hashes) in the output is length `w` and is called a window.

We will now pick out the smallest hash in each window, unless the hash has been picked in a previous window.

```
winnow([H(AB), H(BC), H(CD), H(DE)], w=2)
Output: Set([
	min([H(AB), H(BC)]),
	min([H(BC), H(CD)]),
	min([H(CD), H(DE)]),
])
```

This can be done in linear time using the [sliding window minimum algorithm](https://leetcode.com/problems/sliding-window-maximum/). The twist is that we do not want to pick a minimum in a window if that minimum hash has already been picked in a previous window.

We can choose to remove non-selected hashes in our `HashMap<Hash, (FirstProcessedCharacterIndex, LastProcessedCharacterIndex)>`.

# 4. Comparison

For each student submission, we have a `HashMap<Filename, ([OriginalCharacter], [(ProcessedCharacter, OriginalPosition)], HashMap<Hash, (FirstProcessedCharacterIndex, LastProcessedCharacterIndex)>)>`. Comparison will be done between the hashes of a file to the hashes of all files of all other students.

If the hashes match, we can be pretty sure that the original positions that the hashes in the two files map to represent the plagiarised part of the original code. To avoid false-positives due to collisions, check that `ProcessedCharacters[First]...ProcessedCharacters[Last]` are equal between the two hashes.













