from collections import deque

class RollingHash:
    def __init__(self, k, b, initial_k_characters):
        if k != len(initial_k_characters):
            raise IndexError
        self.k = k
        self.b = b
        self.characters = initial_k_characters 
        self.hash = 0
        for i in range(len(initial_k_characters)):
            c = initial_k_characters[i]
            self.hash += ord(c) * self.b ** (k - i - 1)

    def push(self, c):
        first_char = self.characters.pop(0)
        self.hash -= ord(first_char) * self.b ** (self.k - 1)
        self.hash *= self.b
        self.hash += ord(c)
        self.characters.append(c)
        return self.hash

def get_k_grams(k, b, text):
    if len(text) < k:
        return []
    rolling_hash = RollingHash(k, b, list(text[:k]))
    k_grams = [rolling_hash.hash] + list(map(lambda c: rolling_hash.push(c), text[k:]))
    return k_grams

def maxSlidingWindowIndex(nums, k):
    res = []
    bigger = deque()
    for i, n in enumerate(nums):
        # make sure the rightmost one is the smallest
        while bigger and nums[bigger[-1]] <= n:
            bigger.pop()

        # add in
        bigger += [i]

        # make sure the leftmost one is in-bound
        if i - bigger[0] >= k:
            bigger.popleft()

        # if i + 1 < k, then we are initializing the bigger array
        if i + 1 >= k:
            res.append(bigger[0])
    return res

def winnow(text):
    k = 4
    window_k = 4
    b = 2
    k_grams = get_k_grams(k, b, text)
    selected_k_grams_index = list(set(maxSlidingWindowIndex(k_grams, window_k)))
    selected_k_grams = list(map(lambda i: k_grams[i], selected_k_grams_index))
    return selected_k_grams

def compare(texts):
    k_grams_by_text_index = list(map(lambda k_grams: set(k_grams), map(winnow, texts)))
    k_gram_match_count_by_text_index = [0] * len(texts)
    # count k_gram matches between texts
    for i in range(len(k_grams_by_text_index)):
        for j in range(i + 1, len(k_grams_by_text_index)):
            kgrams_i = k_grams_by_text_index[i]
            kgrams_j = k_grams_by_text_index[j]
            matches = len(kgrams_i & kgrams_j)
            k_gram_match_count_by_text_index[i] += matches
            k_gram_match_count_by_text_index[j] += matches
    return k_gram_match_count_by_text_index

print(compare([
    "The Quick Grey Fox Jumps Over The Lazy Dog",
    "The Quick Brown Fox Jumps Over The Lazy Cat",
    "Hello From the Other Side",
    "The Lazy Cat Meows From The Other Side"
]))