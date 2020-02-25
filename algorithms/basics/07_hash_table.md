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



Searching for a key is the same.

- Advantage:
  - make full use of CPU cache
  - Easy for serialisation

- Disadvantage:
  - Hash table with linear probing can be worse and worse when more and more elements are added.
  -  Delete could be a little bit tricky, one way to do it is instead of delete the key right away, set it as `deleted`.
- Summary: Linear probing is suitable for
  - amount of data is small
  - load factor is small
  - ThreadLocalMap in Java use it.

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

When load factor is larger than a threshold, extend the hash table. But copy a whole hash table can be very time consuming. In stead of moving all of them at once, we can create a new table when load factor is too large. And move one element from old table to the new table every time one new element is added to the new table, until all data is moved. Searching both tables are required when moving process is not finished.

### Chaining

Elements assigned to one space are stored with a linked list

In theory, it's time complexity is O(k), where $k = \#elem / table size$.

- Advantage:
  - Make full use of memory. Ask for memory only when necessary.
- Disadvantage:
  - Linked structure is not friendly to CPU cache (efficiency)
- Optimisation:
  - Linked list to binary search tree/ skip list/ red-black tree which is still O(logn) in worst case
- Summary:
  - Suitable for big object (so the size of an extra pointer can be ignored)
  - and large amount of data



## Industrial level hash table

**Requirements:**

- Fast search, insert and delete
- reasonable memory used
- stable performance, wouldn't lead to unacceptable time complexity

A well designed sequence of data, that makes all of the elements point to the same value could be a way of attack. 

**Implementation:**

1. Know your data.
2. A good but not complex hash function. Distribute keys as even as possible.
   1. 直接寻址法
   2. 平方取中法
   3. 折叠法
   4. 随机数法
3. A good way to solve conflicts

### HashMap in Java

1. Initial size is 16, which could be configured
2. When a chain is longer than 8, will be transformed to a red-black tree. And turn back when nodes less than 8 again. (Because balancing a small red-black tree is not worthy.)