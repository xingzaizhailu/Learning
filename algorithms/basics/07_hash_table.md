# Hash Table

## Hash function

Map a key to a certain index

Three basic elements:

- Hash function must return a non-negative value
- if key1 == key2, hash(key1) == hash(key2)
- if key1 != key2, hash(key1) != hash(key2) (But almost impossible)

## Solve conflicts

Two methods:

- Open addressing
- Chaining

### Open addressing

Idea: find another empty space when conflict happens

#### Linear probing

If the space occupied by another key, then start from current space forward on the array until find an empty space.

```
for i = 0 to n:
		idx = hash1(key) + i
```



Searching for a key is the same. But delete could be a little bit tricky, one way to do it is instead of delete the key right away, set it as `deleted`.

But linear probing can be worse and worse when more and more elements are added.

#### Quadratic probing

```for i = 0 to n:
for i = 0 to n:
		idx = hash1(key) + i * i
```

#### Double hashing

```
for i = 0 to n:
		idx = hash1(key) + i * hash2(key)
```

But no matter which method in open addressing, they all face the problem that conflicts happens more and more frequently when not much space left in hash table.

### Load factor

$ load factor = \frac{\#elem in table}{hash table length}$

### Chaining

Elements assigned to one space are stored with a linked list

In theory, it's time complexity is O(k), where $k = \#elem / table size$.





