#
## Introduction
## Starting Out
### List
``` haskell
> [1,2,3] ++ [4]
> 4:[1,2,3]
```
[1,2,3] is actually just syntactic sugar for 1:2:3:[].  
Get an element out of a list by index, use `!!`. The indices start at 0.

``` haskell
> "Steve Buscemi" !! 6
'B'
```

### Basic funcs on lists
``` haskell
> head [5,4,3,2,1]
5
> tail [5,4,3,2,1]
[4,3,2,1]
> last [5,4,3,2,1]
1
> init [5,4,3,2,1]
[5,4,3,2]
```
Careful funcs above can not be used on empty lists.  

``` haskell
> length [5,4,3,2,1]
5
> null []
True
> reverse [5,4,3,2,1]
[1,2,3,4,5]
> take 3 [5,4,3,2,1]
[5,4,3]
> take 5 [1,2]
[1,2]
> take 0 [1,2]
[]
> drop 4 [8,4,2,1,5,6]
[5,6]
> drop 0 [8,4,2,1,5,6]
[8,4,2,1,5,6]
```

`elem`: takes a thing and a list of things and tell us if that thing is an element of the list.
``` haskell
> 4 `elem` [3,4,5,6]
True
```
More: minimum, maximum, sum, product.  

### Range
``` haskell
> [1..3]
[1,2,3]
> ['a'..'c']
"abc"
> [2, 4..10]
[2,4,6,8,10]
```
To make a list with all the numbers from 20 to 1, you can't just do [20..1], you have to do
[20,19..1].  

Watch out when using floating point numbers in ranges can go wrong!  

``` haskell
take 24 [13,26..]
```

#### Functions to produce inifite lists:
``` haskell
> take 10 (cycle [1,2,3])
[1,2,3,1,2,3,1,2,3,1]
> take 10 (repeat 5)
> replicate 3 10
[10,10,10]
```

### List Comprehension
``` haskell
> [x*2 | x <- [1..10]]
[2,4,6,8,10,12,14,16,18,20]
> [ x | x <- [50..100], x `mod` 7 == 3]
[52,59,66,73,80,87,94]

> [ x*y | x <- [2,5,10], y <- [8,10,11], x*y > 50]
[55,80,100,110]

length' xs = sum [1 | _ <- xs]
```

### Tuples
Tuples are used when you know exactly how many values you want to combine and its type. Unlike a
list, a tuple can contain a combination of several types.

``` haskell
-- fst
> fstl (8, 11)
8

-- snd
> snd (8, 11)
11

-- zip
> zip [1,2,3] [4,5,6]
[(1,4),(2,5),(3,6)]

> zip [1,2,3,4,5,6] ["one","tow","three"] -- even [1..]
[(1,"one"),(2,"two"),(3,"three")]

> let rightTriangles = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^x == c^2, a+b+c == 24]
[(6,8,10)]
```

## Types and Typeclasses
### Believe the type
``` haskell
> :t "Hello"
"Hello" :: [Char]
> :t (True, 'a')
(True, 'a') :: (Bool, Char)
> :t 4==5
4 == 5 :: Bool
```
`::` is read as "has type of".  
It is a good practice to explicit type declaration for functions.

### Common types
- `Int` is bounded, which means that it has a minimum and a maximum value.(64-bit int)  
- `Integer` is not bounded. `Int`, however is more efficient.  
- `Fload` is a real floating point with single precision.(e.g. 25.132742)  
- `Double` is a real floating point with double the precision.(e.g. 25.132741228718345)  
- `Bool` is a boolean type.
- `Char` represents a character. It's denotred by single quotes. A lsit of characters is a string.
- `Tuples` are types but they are dependent on their length as well as the types of their components, so there is theoretically an infinite number of tuple types, which is too many to cover in this tutorial. Note that the empty tuple () is also a type which can only have a single value: ()

### Type variables
``` haskell
> :t head
head :: [a] -> a
```
Because a is not in capital case it's actually a type variable.That means that `a` can be of any type.  
Functions that have type variables are called *polymorphic functions*.

### Typeclasses 101
A typeclass is a sort of interface that defines some behavior.  
- `Eq` is used for types that support equality testing. The functions its members implement are == and /=.  
- `Ord` is for types that have an ordering.  
Members of `Show` can be presented as strings.  
- `Read` is sort of the opposite typeclass of Show. The read function takes a string and returns a
type which is a member of Read.

``` haskell
> read "True" || False
True
> read "8.2" + 3.8
12

> read "5" :: Int
5
> read "(3, 'a')" :: (Int, Char)
```

- `Enum` members are sequentially ordered types — they can be enumerated. The main advantage of the
Enum typeclass is that we can use its types in list ranges. They also have defined successors and
predecesors, which you can get with the succ and pred functions. Types in this class: (), Bool,
Char, Ordering, Int, Integer, Float and Double.

``` haskell
> [LT .. GT]
[LT,EQ,GT]
> succ 'B'
C
```
- `Bounded` members have an upper and a lower bound, have a type of `(Bounded a) => a`.

``` haskell
> minBound :: Bool
False
> maxBound :: Bool
True
> maxBound :: (Bool, Int, Char)
(True,2147483647,'\1114111')
```
- `Num` is a numeric typeclass.

``` haskell
> :t 20
20 :: (Num t) => t
> 20 :: Int
20
ghci> 20 :: Integer
20
ghci> 20 :: Float
20.0
ghci> 20 :: Double
20.0

> :t (*)
(*) :: (Num a) => a -> a -> a
```
- `*` takes two numbers of the same type and returns a number of that type.
`(5 :: Int) * (6 :: Integer)` will result in a type error.

- `Integral` is also a numeric typeclass. Num includes all numbers, including real numbers and
integral numbers, Integral includes only integral (whole) numbers. In this typeclass are Int and
Integer.

- `Floating` includes only floating point numbers, so Float and Double.

``` haskell
fromIntegral :: (Num b, Integral a) => a -> b
```

## Syntax in Functions
### Pattern matching
``` haskell
capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]
```
One more thing — you can't use ++ in pattern matches.
### Guards
### Where
``` haskell
...
where bmi = weight / height ^ 2
`(skinny, normal, fat) = (18.5, 25.0, 30.0)
```
### Let
The difference against `where` is that let bindings are expressions themselves. where bindings are just syntactic constructs.  
If we want to bind to several variables inline, we obviously can't align them at columns. That's why we can separate them with semicolons.

``` haskell
> (let a = 100; b = 200; c = 300 in a*b*c, let foo="Hey "; bar = "there!" in foo ++ bar)  
(6000000,"Hey there!")
```

You can also put let bindings inside list comprehensions.  
``` haskell
calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]
```
We can't use the bmi name in the (w, h) <- xs part because it's defined prior to the let binding.  

We omitted the in part of the let binding when we used them in list comprehensions because the
visibility of the names is already predefined there. However, we could use a let in binding in a
predicate and the names defined would only be visible to that predicate. The in part can also be
omitted when defining functions and constants directly in GHCi. If we do that, then the names will be visible throughout the entire interactive session.  

``` haskell
ghci> let zoot x y z = x * y + z
ghci> zoot 3 9 2
29
ghci> let boot x y z = x * y + z in boot 3 4 2
14
ghci> boot
<interactive>:1:0: Not in scope: `boot'
```
However `let ... in ...` can't span across guards.

### Case expressions
They are useful for pattern matching against something in the middle of an expression. Because
pattern matching in function definitions is syntactic sugar for case expressions, we could have
also defined this like so:

``` haskell
describeList :: [a] -> String
describeList xs = "The list is " ++ what xs
    where what [] = "empty."
          what [x] = "a singleton list."
          what xs = "a longer list."
```
## Recursion
Having an element or two in a recursion definition defined non-recursively (like F(0) and F(1)
here) is also called the `edge condition` and is important if you want your recursive
function to terminate.  

### Maximum awesome
In imperative language, you have to store the maximum value so far. But in haskell:

``` haskell
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs)         -- or just: = max x (maximum' xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum' xs
```

### More recursive functions
- `replicate` :: (Num i, Ord i) => i -> a -> [a]
- `take` :: (Num i, Ord i) => i -> [a] -> [a]
- `reverse'` :: [a] -> [a]
- `repeat'` :: a -> [a]
- `zip'` :: [a] -> [b] -> [(a,b)]
- `elem'` :: (eq a) => a -> [a] -> Bool

### Quick, sort!
``` haskell
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted  = quicksort [a | a <- xs, a >  x]
    in smallerSorted ++ [x] ++ biggerSorted
```

### Thinking Recursively
Usually you define an edge case and then you define a function that does something between some
element and the function applied to the rest.  
When dealing with lists, the edge case is most often the empty list. If you're dealing with trees, the edge case is usually a node that doesn't have any children.

## Higher Order Functions
### Curried functions
### Some higher-orderism is in order
### Maps and filters
### Lambdas
### Only folds and horses
### Function application with $

# TODOOOOO
## Modules
## Making Our Own Types and Typeclasses
## Input and Output
## Functors, Applicative Functors and Monoids
## A Fistful of Monads
## For a Few Monads More
## Zippers
