A while back I encountered a conversation on a platform that shall remain unnamed, where people were recommending must-read books on software engineering and programming. One of the recommendations was [Programming in Lua](https://moys.gov.iq/upload/common/Programming_in_Lua,_4th_ed._(2017)_.pdf) by [Roberto Ierusalimschy](https://en.wikipedia.org/wiki/Roberto_Ierusalimschy), the creator of the Lua programming language. Wishing to brush up on my admittedly rusty imperative programming skills, I decided to take a look at Programming in Lua.

To my surprise and dismay, the first large exercise gave me real trouble! How could it be so difficult to understand code in an introductory text, given that I have been coding for years? The answer of course is that when I do functional programming, there are fundamental differences in how I approach problem solving. For me, the approach taken by the exercise's starting code was very difficult to reason about.

As I was struggling through the exercise, eventually coming to understand how the code worked, a truly cursed idea occurred to me. What if I were to attempt to clone this tiny Lua program in Haskell? I don't mean simply writing a Haskell program that exhibits the same behavior as the Lua code, but imitating the style of the Lua code. This would involve using the parts of Haskell that we are taught to avoid whenever possible, in favor of the easier-to-reason-about functional idioms. It would be an absolutely horrible thing to do. So I decided to do it and write this post about the experience.

The exercise in question is Chapter 2 of the book. The exercise begins with a complete program and then asks the reader to make some modifications to the behavior of the program. This post will cover the Haskell translation of the starting code. If I feel so inclined, I may cover the exercises themselves in future posts. All of the code in this post can be found in this repository. I have doctored the commit history so that you should be able to follow along by stepping through the commits.

The problem presented in this exercise is called the Eight Queen Puzzle. Ierusalimschy describes the puzzle: "its goal is to position eight queens in a chessboard in such a way that no queen can
attack another one" The book further details the problem as follows:

> A first step to solving the eight-queen puzzle is to note that any valid solution must have exactly one queen in each row. Therefore, we can represent potential solutions with a simple array of eight numbers, one for each row; each number tells at which column is the queen at that row. For instance, the array {3, 7, 2, 1, 8, 6, 5, 4} means that the queens are in the squares (1,3), (2,7), (3,2), (4,1), (5,8), (6,6), (7,5), and (8,4). (By the way, this is not a valid solution; for instance, the queen in square (3,2) can attack the one in square (4,1).) Note that any valid solution must be a permutation of the integers 1 to 8, as a valid solution also must have exactly one queen in each column.

Here is the initial solution program in Lua, as it is presented in the book:

```lua
N = 8 -- board size
-- check whether position (n,c) is free from attacks
function isplaceok (a, n, c)
  for i = 1, n - 1 do -- for each queen already placed
    if (a[i] == c) or -- same column?
      (a[i] - i == c - n) or -- same diagonal?
      (a[i] + i == c + n) then -- same diagonal?
      return false -- place can be attacked
    end
  end
  return true -- no attacks; place is OK
end

-- print a board
function printsolution (a)
  for i = 1, N do -- for each row
    for j = 1, N do -- and for each column
      -- write "X" or "-" plus a space
      io.write(a[i] == j and "X" or "-", " ")
    end
    io.write("\n")
  end
  io.write("\n")
end

-- add to board 'a' all queens from 'n' to 'N'
function addqueen (a, n)
  if n > N then -- all queens have been placed?
    printsolution(a)
  else -- try to place n-th queen
    for c = 1, N do
      if isplaceok(a, n, c) then
        a[n] = c -- place n-th queen at column 'c'
        addqueen(a, n + 1)
      end
    end
  end
end

-- run the program
addqueen({}, 1)
```
Ierusalimschy explains the Lua solution:

> The first function is isplaceok, which checks whether a given position on a board is free from attacks from previously placed queens. More specifically, it checks whether putting the n-th queen in column c will conflict with any of the previous n-1 queens already set in the array a. Remember that, by represen- tation, two queens cannot be in the same row, so isplaceok checks whether there are no queens in the same column or in the same diagonals of the new position. Next we have the function printsolution, which prints a board. It simply traverses the entire board, printing an X at positions with a queen and a - at other positions, without any fancy graphics. (Note its use of the and–or idiom to select the character to print at each position.) Each result will look like this:

```
X - - - - - - -
- - - - X - - -
- - - - - - - X
- - - - - X - -
- - X - - - - -
- - - - - - X -
- X - - - - - -
- - - X - - - -
```
> The last function, addqueen, is the core of the program. It tries to place all queens larger than or equal to n in the board. It uses backtracking to search for valid solutions. First, it checks whether the solution is complete and, if so, prints that solution. Otherwise, it loops through all columns for the n-th queen; for each column that is free from attacks, the program places the queen there and recursively tries to place the following queens. Finally, the main body simply calls addqueen on an empty solution.

After reading this explanation, I copied the lua code into my text editor and ran it. I was surprised to see that the program prints not just one, but all of the solutions to the Eight Queen Puzzle! Why did I find it difficult to understand about this program's behavior? At a first glance, I thought that it would print only the first solution it encountered. After all, when the board is full, it hits the recursive "base case" and ends execution after printing the solution. What I failed to notice is that the recursive call in `addqueen` happens _inside a for loop_. So while the execution of that particular recursive tree may have terminated, the for loop in which that recursive function call was made is still running! This is not at all idiomatic in functional programming, even though it is technically possible to put a recursive call inside of a fold.

With a hard-won understanding of how the Lua code works, I started to think about how I could do similarly tough-to-understand things in Haskell. Let's start with the first function, `isplaceok`. It checks if the current position we are looking at is a viable location for the next queen, by looking at the queens that have already been placed. The way this is accomplished in this Lua program is, again, entirely unidiomatic in Haskell. In Haskell I would be inclined to use something like `not $ any (\i -> alreadyPlacedQueenCanAttackCurrentPosition i n c) a`. But here I want to imitate the style of the Lua code, which in this case is using a `for` loop with `return` to short circuit the loop. In Haskell, we don't have loops as language primitives, but we do have the function [`for_`](https://hackage.haskell.org/package/base-4.20.0.1/docs/Data-Foldable.html#v:for_). I decided to use this function to imitate the `for` loop, and the blocking behavior of an `MVar` for the short-circuiting. This seemed like a sufficiently cursed solution to me, so I went with it:

```haskell
isplaceok :: [Int] -> Int -> Int -> MVar Bool -> IO ()
  isplaceok a n c result = do
    for_ [1..(n-1)]
      (\i -> if a !! (i - 1) == c ||
                a !! (i - 1) - i == c - n ||
                a !! (i - 1) + i == c + n
             then putMVar result False -- set the (assumed to be empty) MVar to False
             else pure ()
      )
    _ <- tryPutMVar result True -- If the MVar wasn't set to False anywhere in the forM_ loop, we set it to True here.
    pure ()
```
Compare this with the original Lua:
```lua
function isplaceok (a, n, c)
  for i = 1, n - 1 do -- for each queen already placed
    if (a[i] == c) or -- same column?
      (a[i] - i == c - n) or -- same diagonal?
      (a[i] + i == c + n) then -- same diagonal?
      return false -- place can be attacked
    end
  end
  return true -- no attacks; place is OK
end
```
Note that I must subtract 1 from `i` when indexing because Lua arrays are indexed starting from 1 while Haskell lists are indexed starting from 0.

Next up is the `printsolution` function. This one is pretty straightforward, using for_ in a similar way to how I used it in `isplaceok`.

```haskell
  printsolution :: [Int] -> IO ()
  printsolution a = do
    for_ [1..boardSize] (\i -> do
        for_ [1..boardSize] (\j -> do
            let c = bool "-" "X" $ a !! (i - 1) == j
            putStr $ c ++ " "
          )
        putStr "\n"
      )
    putStr "\n"
```
And here's the Lua:
```lua
-- print a board
function printsolution (a)
  for i = 1, N do -- for each row
    for j = 1, N do -- and for each column
      -- write "X" or "-" plus a space
      io.write(a[i] == j and "X" or "-", " ")
    end
    io.write("\n")
  end
  io.write("\n")
end
```

Finally, we have the `addqueen` function, the core of this program. This function is recursive, so it shouldn't create the mutable variables, but instead take them as arguments. This function takes an MVar that it expects to be empty. The `placeok` function will fill it, and this function will empty it and check the result to determine whether or not to continue. If the result was True, we have found a valid position to place a queen in this row, and we can make the recursive call to move to the next row. If the result was False, we know that the current position is not valid for the queens we have placed so far, so we need to move to the next column. To do this, we don't make a recursive call and allow the `for` loop to continue checking the index for the next column. Also of note is that I needed to define a `set` function to update a value in a list, even if it is out of the current bounds of the list. I wrote this `set` to take an index starting from 1, like in Lua.
```haskell
addqueen :: MVar Bool -> IORef [Int] -> Int -> IO ()
addqueen placeokVar aRef n =
  if n > boardSize
  then readIORef aRef >>= printsolution
  else for_ [1..boardSize] (\c -> do
           a <- readIORef aRef
           isplaceok a n c placeokVar
           placeok <- takeMVar placeokVar
           if placeok then do
             modifyIORef aRef $ set n c
             addqueen placeokVar aRef (n + 1)
           else pure ()
         )
  where
    set :: Int -> Int -> [Int] -> [Int]
    set i v l = take (i - 1) l ++ [v] ++ drop i l
```
Finally, I just needed to define a main function! This is where I initialize the mutable variables, before passing them to the `addqueen` function.
```haskell
main :: IO ()
main = do
  placeokVar <- newEmptyMVar
  aRef <- newIORef []
  addqueen placeokVar aRef 1
```
With that code in place, I crossed my fingers and ran the code
```
> runhaskell haskell/eight-queen-from-text-clone.hs
```
To my dismay, it hung, and then printed out this error:
```
eight-queen-from-text-clone.hs: thread blocked indefinitely in an MVar operation
```
Nooooo! There was something wrong in my reasoning about my use of MVars. If you already spotted the flaw in my logic, give yourself a pat on the back! I, on the other hand, was a bit puzzled, so I decided to put in some logging to debug the issue.
```haskell
addqueen :: MVar Bool -> IORef [Int] -> Int -> IO ()
addqueen placeokVar aRef n =
  if n > boardSize
  then readIORef aRef >>= printsolution
  else for_ [1..boardSize] (\c -> do
           a <- readIORef aRef
           log 1 a n c
           isplaceok a n c placeokVar
           log 2 a n c
           placeok <- takeMVar placeokVar
           log 3 a n c
           if placeok then do
             modifyIORef aRef $ set n c
             log 4 a n c
             addqueen placeokVar aRef (n + 1)
           else pure ()
         )
  where
     set :: Int -> Int -> [Int] -> [Int]
     set i v l = take (i - 1) l ++ [v] ++ drop i l
     log :: Int -> [Int] -> Int -> Int -> IO ()
     log i a n c =
       let message =
             show i ++ ": a = "
             ++ show a ++ ", n = "
             ++ show n ++ ", c = "
             ++ show c
       in putStrLn message
```
This resulted in the following output:
```
> runhaskell haskell/eight-queen-from-text-clone.hs
1: a = [], n = 1, c = 1
2: a = [], n = 1, c = 1
3: a = [], n = 1, c = 1
4: a = [], n = 1, c = 1
1: a = [1], n = 2, c = 1
2: a = [1], n = 2, c = 1
3: a = [1], n = 2, c = 1
1: a = [1], n = 2, c = 2
2: a = [1], n = 2, c = 2
3: a = [1], n = 2, c = 2
1: a = [1], n = 2, c = 3
2: a = [1], n = 2, c = 3
3: a = [1], n = 2, c = 3
4: a = [1], n = 2, c = 3
1: a = [1,3], n = 3, c = 1
2: a = [1,3], n = 3, c = 1
3: a = [1,3], n = 3, c = 1
1: a = [1,3], n = 3, c = 2
2: a = [1,3], n = 3, c = 2
3: a = [1,3], n = 3, c = 2
1: a = [1,3], n = 3, c = 3
eight-queen-from-text-clone.hs: thread blocked indefinitely in an MVar operation
```
I stared at these log results and didn't quite know what to make of it. I ran it a few times more to ensure that I had it right, and the output was consistently the same. There was no flakiness to be found. After a bit of puzzling over it I was able to make a couple of conclusions. First, my MVar logic, at least with regard to the interaction between `addqueen` and `isplaceok`, was not fundamentally flawed, as the MVar was being successfully filled and emptied for six calls to `addqueen`. We can tell that there are six successful passes of the MVar from `addqueen` to `isplaceok` and back, because that is how many times we see log 1 followed by log 2, and the call to `isplaceok` happens between those two logging statements. Second, the blocking of the MVar must be happening in the seventh call to `isplaceok`, as it is the seventh time that log 1 is printed that the program hangs and crashes. I still couldn't see how `isplaceok` could be blocking the MVar, so I decided to add some logging to that function as well:
```haskell
isplaceok :: [Int] -> Int -> Int -> MVar Bool -> IO ()
isplaceok a n c result = do
  for_ [1..(n-1)]
    (\i -> do
           log i
           if a !! (i - 1) == c ||
              a !! (i - 1) - i == c - n ||
              a !! (i - 1) + i == c + n
           then putMVar result False
           else pure ()
    )
  _ <- tryPutMVar result True
  pure ()
  where
    log :: Int -> IO ()
    log i =
      let message = "i = " ++ show i
      in putStrLn message
```
With this new logging in place, I ran the program again and saw this output:
```
> runhaskell haskell/eight-queen-from-text-clone.hs
1: a = [], n = 1, c = 1
2: a = [], n = 1, c = 1
3: a = [], n = 1, c = 1
4: a = [], n = 1, c = 1
1: a = [1], n = 2, c = 1
i = 1
2: a = [1], n = 2, c = 1
3: a = [1], n = 2, c = 1
1: a = [1], n = 2, c = 2
i = 1
2: a = [1], n = 2, c = 2
3: a = [1], n = 2, c = 2
1: a = [1], n = 2, c = 3
i = 1
2: a = [1], n = 2, c = 3
3: a = [1], n = 2, c = 3
4: a = [1], n = 2, c = 3
1: a = [1,3], n = 3, c = 1
i = 1
i = 2
2: a = [1,3], n = 3, c = 1
3: a = [1,3], n = 3, c = 1
1: a = [1,3], n = 3, c = 2
i = 1
i = 2
2: a = [1,3], n = 3, c = 2
3: a = [1,3], n = 3, c = 2
1: a = [1,3], n = 3, c = 3
i = 1
i = 2
eight-queen-from-text-clone.hs: thread blocked indefinitely in an MVar operation
```
Seeing the different indices in the `isplaceok` function printed out, finally helped me see the flaw in my logic. The blocking functionality of the MVar does nothing to actually make the `for_` "loop" short circuit! The MVar turned out to be a good choice for the style, as it forced me to make sure that I was making a loop that would short circuit, but it in itself was not causing the short circuit. Rather, it was blocking execution when the loop failed to short circuit and there were more than one already-placed queen threatening the spot we are checking.

Let's break this down a little further so there's no confusion. To re-clarify, the logic of `isplaceok` determines if a candidate position in row `n`, column `c`, is threatened by any of the already placed queens. It does this by checking each of the already placed queens in the array `a`. For each of the placed queens, it makes three checks. First it checks if the candidate position is in the same column: `a !! (i - 1) == c`. This is checking the queen placed in the `i`th position of `a` (`i` denoting the row). Remember that `i` is a base-1-index as in Lua, but `a` is a base-0-indexed Haskell list. (Please note that I'm using the word base here to denote where we start counting from, not the number system of the indices). Next it checks if the candidate position is in the same diagonal running top-left to bottom-right: `a !! (i - 1) - i == c - n`. Finally, it checks if the candidate position is in the same diagonal running top-right to bottom-left: `a !! (i - 1) + i == c + n`.

The problematic case, as we can see from the logs, has `a = [1,3]`, `n = 3`, and `c = 3`. The value of `a` means that we have already placed a queen at row 1 column 1, and also at row 2 column 3. The values of `n` and `c` mean that the candidate position is at row 3 column 3. The candidate position is threatened on the diagonal by the first queen, so the second condition resolves to True:
```haskell
a !! (i - 1) - i == c - n
-- | - substitute 1 in for i, 3 for c and 3 for n
-- v
a !! (1 - 1) - 1 == 3 - 3
-- | - evaluate within the parenthesis
-- v
a !! 0 - 1 == 3 - 3
-- | - get the value in `a` at Haskell list index 0
-- v
1 - 1 == 3 - 3`
-- | - perform the subtractions
-- v
0 == 0
-- | - evaluate the equality
-- v
True
```
As a result, we fill the MVar with the value False: `putMVar result False`. The MVar `result` is now blocked on further puts until `takeMVar` gets called, but `for_` is not done processing the list `a`! It continues for the second value of `i`. As it turns out, the second queen, at row 2 column 3, also threatens the candidate position. It is in the same column, so the first condition resolves to True:
```haskell
a !! (i - 1) == c
-- | - substitute 1 in for i and 3 for c
-- v
a !! (2 - 1) == 3
-- | - evaluate within the parenthesis
-- v
a !! 1 == 3
-- | - get the value in `a` at Haskell list index 1
-- v
3 == 3
-- | - evaluate the equality
-- v
True
```
Once again, we try to put a False value in the `result` MVar: `putMVar result False`. This time though, the computation is blocked, because the MVar is already full. There is no asynchronous computation to empty the `result`, so the program blocks here, and crashes.

Now I finally understood the error of my logic and I was faced with a new problem. How can I make a for loop that short circuits like a Lua for loop does? First I took a look at the definition of [`for_`](https://hackage.haskell.org/package/base-4.20.0.1/docs/Data-Foldable.html#v:for_) to see if I could use it any differently.
```haskell
for_ :: [Int] -> (Int -> IO ()) -> IO ()
for_ = flip traverse_
```
Okay, that doesn't tell me much. So what is the definition of [`traverse_`](https://hackage.haskell.org/package/base-4.20.0.1/docs/Data-Foldable.html#v:traverse_)?
```haskell
traverse_ :: (Int -> IO ()) -> [Int] -> IO ()
traverse_ f = foldr c (pure ())
  where c x k = f x *> k
```
I know that `foldr` will short-circuit when the function passed to it doesn't evaluate its second argument. However, the function passed to `foldr` here, `c`, always evaluates its second argument, `k`. This means I won't be able to use `for_` to imitate lua for loops. However, I might be able write a similar combinator that _does_ short-circuit using `foldr`.

For those who are unfamiliar with this behavior of `foldr`, let's go over a relatively simple example. We can implement the [`take`](https://hackage.haskell.org/package/base-4.20.0.1/docs/Prelude.html#v:take) function, that takes the first n elements of a list, in terms of `foldr` as follows:
```haskell
take :: Int -> [a] -> [a]
take n = map fst $ foldr f  [] $ zip l [0..]
  where
    f :: (a, Int) -> [(a, Int)] -> [(a, Int)]
    f tuple@(a, n') l =
      if n' < n then tuple : l else []
```
This implementation will stop as soon as the condition in `f` is false, which means it can be called on an infinite Haskell list. To see how this happens let's see how `take 2 [1,2,3,4]` gets calculated for the following definition of [`foldr`](https://hackage.haskell.org/package/base-4.20.0.1/docs/Prelude.html#v:foldr) (there are many ways to define `foldr`, but this one is relatively easy to reason about):
```haskell
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z = go
  where
    go [] = z
    go (x:xs) = x `f` go xs
```
The expression evaluates as follows:
```haskell
take 2 [1,2,3,4]
-- | - apply definition of `take` and `zip`
-- v
map fst $ foldr f [] [(1,0), (2,1), (3,2), (4,3)]
  where
    f tuple@(a, n') l =
      if n' < 2 then tuple : l else []
-- | - apply definition of `foldr`
-- v
map fst $ go [(1,0), (2,1), (3,2), (4,3)]
  where
    go [] = []
    go (x:xs) = x `f` go xs
    f tuple@(a, n') l =
      if n' < 2 then tuple : l else []
-- | - (From this point on I will elide the definitions
-- | - of `f` and `go`, as they don't change).
-- | - first application of `go` to the tuple list
-- v
map fst $ (1,0) `f` go [(2,1), (3,2), (4,3)]
-- | - apply `f`
-- v
map fst $ (1,0) : go [(2,1), (3,2), (4,3)]
-- | - apply `go` 
-- v
map fst $ (1,0) : (2,1) `f` go [(3,2), (4,3)]
-- | - apply `f`
-- v
map fst $ (1,0) : (2,1) : go [(3,2), (4,3)]
-- | - apply `go`
-- v
map fst $ (1,0) : (2,1) : (3,2) `f` go [(4,3)]
-- | - apply `f`
-- | - `2 < 2` is false, so `f` returns an empty list
-- v
map fst $ (1,0) : (2,1) : []
-- | - list syntactic sugar
-- v
map fst $ [(1,0), (2,1)]
-- | - apply `map fst`
-- v
[1,2]
```
As we see in this example, `foldr` will only continue computation if the function `f` passed to it evaluates its second argument. When we reached the index (the second tuple element) that was not smaller than the argument `n`, the function `f` did not evaluate its second argument, and instead returned an empty list. This halted computation, because there were no more calls to `go` in the expression.

We can use this property of `foldr` to make a `for` function that behaves much more like a for loop in Lua. Since we are making our own function, we can take two numbers instead of a list, to be a little closer to the syntax of a Lua `for` loop.
```haskell
for :: Int -> Int -> (Int -> IO () -> IO ()) -> IO ()
for i n action = foldr action (pure ()) [i..n]
```
Notice that the action this combinator takes has two arguments, not one as with for_. We can think of this second argument as the rest of the computation that will be computed if we continue processing the list. When I use this new `for` combinator, I name this argument `continue`. With this naming convention, we can see one interesting difference between the Lua `for` loop and my Haskell `for` function. In a Lua `for` loop we must explicitly _end_ the loop computation, with the `return` or `break` keywords, while with my `for` function, we must explicitly _remain in_ the loop by calling the `continue` argument in the definition of the `action`. With this in mind, here is the new and improved `isplaceok` function:
```haskell
isplaceok :: [Int] -> Int -> Int -> MVar Bool -> IO ()
isplaceok a n c result = do
  for 1 (n - 1)
    (\i continue ->
      if a !! (i - 1) == c ||
         a !! (i - 1) - i == c - n ||
         a !! (i - 1) + i == c + n
      then putMVar result False
      else continue
    )
  _ <- tryPutMVar result True
  pure ()
```
Notice that now, in the case that we put `False` in the `result` MVar, we do not call `continue`, meaning the loop computation will end. I decided to use my new `for` combinator in `addqueen` as well:
```haskell
addqueen :: MVar Bool -> IORef [Int] -> Int -> IO ()
addqueen placeokVar aRef n =
  if n > boardSize
  then readIORef aRef >>= printsolution
  else for 1 boardSize (\c continue -> do
           a <- readIORef aRef
           isplaceok a n c placeokVar
           placeok <- takeMVar placeokVar
           if placeok then do
             modifyIORef aRef $ set n c
             addqueen placeokVar aRef (n + 1)
           else pure ()
           continue
         )
  where
    set :: Int -> Int -> [Int] -> [Int]
    set i v l = take (i - 1) l ++ [v] ++ drop i l
```
This didn't change the behavior of `addqueen`, but it felt nice. I left the definition of `printsolution` as it was for no particular reason. Here is the final program:
```haskell
module Main where

import Control.Concurrent.MVar
import Data.Bool (bool)
import Data.Foldable (for_)
import Data.IORef

boardSize :: Int
boardSize = 8

for :: Int -> Int -> (Int -> IO () -> IO ()) -> IO ()
for i n action = foldr action (pure ()) [i..n]

isplaceok :: [Int] -> Int -> Int -> MVar Bool -> IO ()
isplaceok a n c result = do
  for 1 (n - 1)
    (\i continue ->
      if a !! (i - 1) == c ||
         a !! (i - 1) - i == c - n ||
         a !! (i - 1) + i == c + n
      then putMVar result False
      else continue
    )
  _ <- tryPutMVar result True
  pure ()

printsolution :: [Int] -> IO ()
printsolution a = do
  for_ [1..boardSize] (\i -> do
      for_ [1..boardSize] (\j -> do
          let c = bool "-" "X" $ a !! (i - 1) == j
          putStr $ c ++ " "
        )
      putStr "\n"
    )
  putStr "\n"

addqueen :: MVar Bool -> IORef [Int] -> Int -> IO ()
addqueen placeokVar aRef n =
  if n > boardSize
  then readIORef aRef >>= printsolution
  else for 1 boardSize (\c continue -> do
           a <- readIORef aRef
           isplaceok a n c placeokVar
           placeok <- takeMVar placeokVar
           if placeok then do
             modifyIORef aRef $ set n c
             addqueen placeokVar aRef (n + 1)
           else pure ()
           continue
         )
  where
    set :: Int -> Int -> [Int] -> [Int]
    set i v l = take (i - 1) l ++ [v] ++ drop i l

main :: IO ()
main = do
  placeokVar <- newEmptyMVar
  aRef <- newIORef []
  addqueen placeokVar aRef 1
```
With the critical modification in `isplaceok` implemented, my Haskell translation finally gave the same output as the Lua program from the book:
```
> runhaskell haskell/eight-queen-from-text-clone.hs
X - - - - - - -
- - - - X - - -
- - - - - - - X
- - - - - X - -
- - X - - - - -
- - - - - - X -
- X - - - - - -
- - - X - - - -

X - - - - - - -
- - - - - X - -
- - - - - - - X
- - X - - - - -
- - - - - - X -
- - - X - - - -
- X - - - - - -
- - - - X - - -

X - - - - - - -
- - - - - - X -
- - - X - - - -
- - - - - X - -
- - - - - - - X
- X - - - - - -
- - - - X - - -
- - X - - - - -

X - - - - - - -
- - - - - - X -
- - - - X - - -
- - - - - - - X
- X - - - - - -
- - - X - - - -
- - - - - X - -
- - X - - - - -

- X - - - - - -
- - - X - - - -
- - - - - X - -
- - - - - - - X
- - X - - - - -
X - - - - - - -
- - - - - - X -
- - - - X - - -

- X - - - - - -
- - - - X - - -
- - - - - - X -
X - - - - - - -
- - X - - - - -
- - - - - - - X
- - - - - X - -
- - - X - - - -

- X - - - - - -
- - - - X - - -
- - - - - - X -
- - - X - - - -
X - - - - - - -
- - - - - - - X
- - - - - X - -
- - X - - - - -

- X - - - - - -
- - - - - X - -
X - - - - - - -
- - - - - - X -
- - - X - - - -
- - - - - - - X
- - X - - - - -
- - - - X - - -

- X - - - - - -
- - - - - X - -
- - - - - - - X
- - X - - - - -
X - - - - - - -
- - - X - - - -
- - - - - - X -
- - - - X - - -

- X - - - - - -
- - - - - - X -
- - X - - - - -
- - - - - X - -
- - - - - - - X
- - - - X - - -
X - - - - - - -
- - - X - - - -

- X - - - - - -
- - - - - - X -
- - - - X - - -
- - - - - - - X
X - - - - - - -
- - - X - - - -
- - - - - X - -
- - X - - - - -

- X - - - - - -
- - - - - - - X
- - - - - X - -
X - - - - - - -
- - X - - - - -
- - - - X - - -
- - - - - - X -
- - - X - - - -

- - X - - - - -
X - - - - - - -
- - - - - - X -
- - - - X - - -
- - - - - - - X
- X - - - - - -
- - - X - - - -
- - - - - X - -

- - X - - - - -
- - - - X - - -
- X - - - - - -
- - - - - - - X
X - - - - - - -
- - - - - - X -
- - - X - - - -
- - - - - X - -

- - X - - - - -
- - - - X - - -
- X - - - - - -
- - - - - - - X
- - - - - X - -
- - - X - - - -
- - - - - - X -
X - - - - - - -

- - X - - - - -
- - - - X - - -
- - - - - - X -
X - - - - - - -
- - - X - - - -
- X - - - - - -
- - - - - - - X
- - - - - X - -

- - X - - - - -
- - - - X - - -
- - - - - - - X
- - - X - - - -
X - - - - - - -
- - - - - - X -
- X - - - - - -
- - - - - X - -

- - X - - - - -
- - - - - X - -
- X - - - - - -
- - - - X - - -
- - - - - - - X
X - - - - - - -
- - - - - - X -
- - - X - - - -

- - X - - - - -
- - - - - X - -
- X - - - - - -
- - - - - - X -
X - - - - - - -
- - - X - - - -
- - - - - - - X
- - - - X - - -

- - X - - - - -
- - - - - X - -
- X - - - - - -
- - - - - - X -
- - - - X - - -
X - - - - - - -
- - - - - - - X
- - - X - - - -

- - X - - - - -
- - - - - X - -
- - - X - - - -
X - - - - - - -
- - - - - - - X
- - - - X - - -
- - - - - - X -
- X - - - - - -

- - X - - - - -
- - - - - X - -
- - - X - - - -
- X - - - - - -
- - - - - - - X
- - - - X - - -
- - - - - - X -
X - - - - - - -

- - X - - - - -
- - - - - X - -
- - - - - - - X
X - - - - - - -
- - - X - - - -
- - - - - - X -
- - - - X - - -
- X - - - - - -

- - X - - - - -
- - - - - X - -
- - - - - - - X
X - - - - - - -
- - - - X - - -
- - - - - - X -
- X - - - - - -
- - - X - - - -

- - X - - - - -
- - - - - X - -
- - - - - - - X
- X - - - - - -
- - - X - - - -
X - - - - - - -
- - - - - - X -
- - - - X - - -

- - X - - - - -
- - - - - - X -
- X - - - - - -
- - - - - - - X
- - - - X - - -
X - - - - - - -
- - - X - - - -
- - - - - X - -

- - X - - - - -
- - - - - - X -
- X - - - - - -
- - - - - - - X
- - - - - X - -
- - - X - - - -
X - - - - - - -
- - - - X - - -

- - X - - - - -
- - - - - - - X
- - - X - - - -
- - - - - - X -
X - - - - - - -
- - - - - X - -
- X - - - - - -
- - - - X - - -

- - - X - - - -
X - - - - - - -
- - - - X - - -
- - - - - - - X
- X - - - - - -
- - - - - - X -
- - X - - - - -
- - - - - X - -

- - - X - - - -
X - - - - - - -
- - - - X - - -
- - - - - - - X
- - - - - X - -
- - X - - - - -
- - - - - - X -
- X - - - - - -

- - - X - - - -
- X - - - - - -
- - - - X - - -
- - - - - - - X
- - - - - X - -
X - - - - - - -
- - X - - - - -
- - - - - - X -

- - - X - - - -
- X - - - - - -
- - - - - - X -
- - X - - - - -
- - - - - X - -
- - - - - - - X
X - - - - - - -
- - - - X - - -

- - - X - - - -
- X - - - - - -
- - - - - - X -
- - X - - - - -
- - - - - X - -
- - - - - - - X
- - - - X - - -
X - - - - - - -

- - - X - - - -
- X - - - - - -
- - - - - - X -
- - - - X - - -
X - - - - - - -
- - - - - - - X
- - - - - X - -
- - X - - - - -

- - - X - - - -
- X - - - - - -
- - - - - - - X
- - - - X - - -
- - - - - - X -
X - - - - - - -
- - X - - - - -
- - - - - X - -

- - - X - - - -
- X - - - - - -
- - - - - - - X
- - - - - X - -
X - - - - - - -
- - X - - - - -
- - - - X - - -
- - - - - - X -

- - - X - - - -
- - - - - X - -
X - - - - - - -
- - - - X - - -
- X - - - - - -
- - - - - - - X
- - X - - - - -
- - - - - - X -

- - - X - - - -
- - - - - X - -
- - - - - - - X
- X - - - - - -
- - - - - - X -
X - - - - - - -
- - X - - - - -
- - - - X - - -

- - - X - - - -
- - - - - X - -
- - - - - - - X
- - X - - - - -
X - - - - - - -
- - - - - - X -
- - - - X - - -
- X - - - - - -

- - - X - - - -
- - - - - - X -
X - - - - - - -
- - - - - - - X
- - - - X - - -
- X - - - - - -
- - - - - X - -
- - X - - - - -

- - - X - - - -
- - - - - - X -
- - X - - - - -
- - - - - - - X
- X - - - - - -
- - - - X - - -
X - - - - - - -
- - - - - X - -

- - - X - - - -
- - - - - - X -
- - - - X - - -
- X - - - - - -
- - - - - X - -
X - - - - - - -
- - X - - - - -
- - - - - - - X

- - - X - - - -
- - - - - - X -
- - - - X - - -
- - X - - - - -
X - - - - - - -
- - - - - X - -
- - - - - - - X
- X - - - - - -

- - - X - - - -
- - - - - - - X
X - - - - - - -
- - X - - - - -
- - - - - X - -
- X - - - - - -
- - - - - - X -
- - - - X - - -

- - - X - - - -
- - - - - - - X
X - - - - - - -
- - - - X - - -
- - - - - - X -
- X - - - - - -
- - - - - X - -
- - X - - - - -

- - - X - - - -
- - - - - - - X
- - - - X - - -
- - X - - - - -
X - - - - - - -
- - - - - - X -
- X - - - - - -
- - - - - X - -

- - - - X - - -
X - - - - - - -
- - - X - - - -
- - - - - X - -
- - - - - - - X
- X - - - - - -
- - - - - - X -
- - X - - - - -

- - - - X - - -
X - - - - - - -
- - - - - - - X
- - - X - - - -
- X - - - - - -
- - - - - - X -
- - X - - - - -
- - - - - X - -

- - - - X - - -
X - - - - - - -
- - - - - - - X
- - - - - X - -
- - X - - - - -
- - - - - - X -
- X - - - - - -
- - - X - - - -

- - - - X - - -
- X - - - - - -
- - - X - - - -
- - - - - X - -
- - - - - - - X
- - X - - - - -
X - - - - - - -
- - - - - - X -

- - - - X - - -
- X - - - - - -
- - - X - - - -
- - - - - - X -
- - X - - - - -
- - - - - - - X
- - - - - X - -
X - - - - - - -

- - - - X - - -
- X - - - - - -
- - - - - X - -
X - - - - - - -
- - - - - - X -
- - - X - - - -
- - - - - - - X
- - X - - - - -

- - - - X - - -
- X - - - - - -
- - - - - - - X
X - - - - - - -
- - - X - - - -
- - - - - - X -
- - X - - - - -
- - - - - X - -

- - - - X - - -
- - X - - - - -
X - - - - - - -
- - - - - X - -
- - - - - - - X
- X - - - - - -
- - - X - - - -
- - - - - - X -

- - - - X - - -
- - X - - - - -
X - - - - - - -
- - - - - - X -
- X - - - - - -
- - - - - - - X
- - - - - X - -
- - - X - - - -

- - - - X - - -
- - X - - - - -
- - - - - - - X
- - - X - - - -
- - - - - - X -
X - - - - - - -
- - - - - X - -
- X - - - - - -

- - - - X - - -
- - - - - - X -
X - - - - - - -
- - X - - - - -
- - - - - - - X
- - - - - X - -
- - - X - - - -
- X - - - - - -

- - - - X - - -
- - - - - - X -
X - - - - - - -
- - - X - - - -
- X - - - - - -
- - - - - - - X
- - - - - X - -
- - X - - - - -

- - - - X - - -
- - - - - - X -
- X - - - - - -
- - - X - - - -
- - - - - - - X
X - - - - - - -
- - X - - - - -
- - - - - X - -

- - - - X - - -
- - - - - - X -
- X - - - - - -
- - - - - X - -
- - X - - - - -
X - - - - - - -
- - - X - - - -
- - - - - - - X

- - - - X - - -
- - - - - - X -
- X - - - - - -
- - - - - X - -
- - X - - - - -
X - - - - - - -
- - - - - - - X
- - - X - - - -

- - - - X - - -
- - - - - - X -
- - - X - - - -
X - - - - - - -
- - X - - - - -
- - - - - - - X
- - - - - X - -
- X - - - - - -

- - - - X - - -
- - - - - - - X
- - - X - - - -
X - - - - - - -
- - X - - - - -
- - - - - X - -
- X - - - - - -
- - - - - - X -

- - - - X - - -
- - - - - - - X
- - - X - - - -
X - - - - - - -
- - - - - - X -
- X - - - - - -
- - - - - X - -
- - X - - - - -

- - - - - X - -
X - - - - - - -
- - - - X - - -
- X - - - - - -
- - - - - - - X
- - X - - - - -
- - - - - - X -
- - - X - - - -

- - - - - X - -
- X - - - - - -
- - - - - - X -
X - - - - - - -
- - X - - - - -
- - - - X - - -
- - - - - - - X
- - - X - - - -

- - - - - X - -
- X - - - - - -
- - - - - - X -
X - - - - - - -
- - - X - - - -
- - - - - - - X
- - - - X - - -
- - X - - - - -

- - - - - X - -
- - X - - - - -
X - - - - - - -
- - - - - - X -
- - - - X - - -
- - - - - - - X
- X - - - - - -
- - - X - - - -

- - - - - X - -
- - X - - - - -
X - - - - - - -
- - - - - - - X
- - - X - - - -
- X - - - - - -
- - - - - - X -
- - - - X - - -

- - - - - X - -
- - X - - - - -
X - - - - - - -
- - - - - - - X
- - - - X - - -
- X - - - - - -
- - - X - - - -
- - - - - - X -

- - - - - X - -
- - X - - - - -
- - - - X - - -
- - - - - - X -
X - - - - - - -
- - - X - - - -
- X - - - - - -
- - - - - - - X

- - - - - X - -
- - X - - - - -
- - - - X - - -
- - - - - - - X
X - - - - - - -
- - - X - - - -
- X - - - - - -
- - - - - - X -

- - - - - X - -
- - X - - - - -
- - - - - - X -
- X - - - - - -
- - - X - - - -
- - - - - - - X
X - - - - - - -
- - - - X - - -

- - - - - X - -
- - X - - - - -
- - - - - - X -
- X - - - - - -
- - - - - - - X
- - - - X - - -
X - - - - - - -
- - - X - - - -

- - - - - X - -
- - X - - - - -
- - - - - - X -
- - - X - - - -
X - - - - - - -
- - - - - - - X
- X - - - - - -
- - - - X - - -

- - - - - X - -
- - - X - - - -
X - - - - - - -
- - - - X - - -
- - - - - - - X
- X - - - - - -
- - - - - - X -
- - X - - - - -

- - - - - X - -
- - - X - - - -
- X - - - - - -
- - - - - - - X
- - - - X - - -
- - - - - - X -
X - - - - - - -
- - X - - - - -

- - - - - X - -
- - - X - - - -
- - - - - - X -
X - - - - - - -
- - X - - - - -
- - - - X - - -
- X - - - - - -
- - - - - - - X

- - - - - X - -
- - - X - - - -
- - - - - - X -
X - - - - - - -
- - - - - - - X
- X - - - - - -
- - - - X - - -
- - X - - - - -

- - - - - X - -
- - - - - - - X
- X - - - - - -
- - - X - - - -
X - - - - - - -
- - - - - - X -
- - - - X - - -
- - X - - - - -

- - - - - - X -
X - - - - - - -
- - X - - - - -
- - - - - - - X
- - - - - X - -
- - - X - - - -
- X - - - - - -
- - - - X - - -

- - - - - - X -
- X - - - - - -
- - - X - - - -
X - - - - - - -
- - - - - - - X
- - - - X - - -
- - X - - - - -
- - - - - X - -

- - - - - - X -
- X - - - - - -
- - - - - X - -
- - X - - - - -
X - - - - - - -
- - - X - - - -
- - - - - - - X
- - - - X - - -

- - - - - - X -
- - X - - - - -
X - - - - - - -
- - - - - X - -
- - - - - - - X
- - - - X - - -
- X - - - - - -
- - - X - - - -

- - - - - - X -
- - X - - - - -
- - - - - - - X
- X - - - - - -
- - - - X - - -
X - - - - - - -
- - - - - X - -
- - - X - - - -

- - - - - - X -
- - - X - - - -
- X - - - - - -
- - - - X - - -
- - - - - - - X
X - - - - - - -
- - X - - - - -
- - - - - X - -

- - - - - - X -
- - - X - - - -
- X - - - - - -
- - - - - - - X
- - - - - X - -
X - - - - - - -
- - X - - - - -
- - - - X - - -

- - - - - - X -
- - - - X - - -
- - X - - - - -
X - - - - - - -
- - - - - X - -
- - - - - - - X
- X - - - - - -
- - - X - - - -

- - - - - - - X
- X - - - - - -
- - - X - - - -
X - - - - - - -
- - - - - - X -
- - - - X - - -
- - X - - - - -
- - - - - X - -

- - - - - - - X
- X - - - - - -
- - - - X - - -
- - X - - - - -
X - - - - - - -
- - - - - - X -
- - - X - - - -
- - - - - X - -

- - - - - - - X
- - X - - - - -
X - - - - - - -
- - - - - X - -
- X - - - - - -
- - - - X - - -
- - - - - - X -
- - - X - - - -

- - - - - - - X
- - - X - - - -
X - - - - - - -
- - X - - - - -
- - - - - X - -
- X - - - - - -
- - - - - - X -
- - - - X - - -
```
What have I learned from this experience? That with a few terrible decisions and using the parts of Haskell we usually try to avoid at all cost, it is possible to get pretty close to the style of a basic Lua program. Maybe a little too close for comfort.
