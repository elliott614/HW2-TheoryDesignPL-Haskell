-- CS 839, Spring 2019: Homework 2
-- Part 1: Append lists (30)

-- Do not change the following line!
{-# OPTIONS -fwarn-incomplete-patterns -fwarn-tabs -fno-warn-type-defaults #-}

-- **Your code should compile without warnings.**
-- The following line makes the compiler treat all warnings as hard errors.
-- When you are done, uncomment it and fix until there are no more errors.
-- (You may ignore unused function warnings.)
{-# OPTIONS -Wall -Werror #-}

-- You might want some functions from these libraries:
-- Data.List
--
-- In the functional programming style, we usually avoid mutating or updating
-- memory cells, preferring instead pure operations. Accordingly, data
-- structures in functional languages look rather different from the data
-- structures you might be used to. In this assignment, you will write a few
-- purely functional data structures.

-- Concatenating lists with `(++)` is an expensive operation. It is linear in
-- the length of the first argument, which must be "unravelled" and then added
-- back on one at a time. 
-- To improve this, let's consider AppendLists. These use higher-order functions
-- so that appending becomes a constant-time operation. The price is that the
-- other list operations are much less efficient.
--
-- Note that for some of these functions, the best solution is just to convert
-- the append list to a regular list, perform the list operation there, and then
-- convert back to an append list. This is unavoidable for some operations, but
-- try to see if you can perform the operation more efficiently before reaching
-- for regular lists (e.g., for append).

-- The elements of the list will be of type `a`. 
newtype AppendList a = AList ([a] -> [a])

-- The empty list is represented using the identity function, by
-- AList (\x -> x)
empty :: AppendList a
empty = AList id

-- The list [3] is represented as AList (\x -> 3:x)
-- The list [1, 9, 4] will be represented as the function (\x -> 1 : 9: 4 : x).

-- Based on these examples, write a function that takes on argument and returns
-- the AList that corresponds to the list with just that argument as element
singleton :: a -> AppendList a
singleton a = AList (a :)

-- Write a function that prepends an element to any appendlist, just like cons
-- (written : ) does for lists
alistCons :: a -> AppendList a -> AppendList a
alistCons a (AList b) = AList (\x -> a : b x)

-- Write foldr and map functions for AppendLists, just like for Lists
alistFoldr :: (a -> b -> b) -> b -> AppendList a -> b
alistFoldr f b a = foldr f b (toList a)

-- Try to write this without using fromList
alistMap :: (a -> b) -> AppendList a -> AppendList b
alistMap f a = alistConcat $ map (singleton . f) (toList a)

-- Finally, write the function that motivated all of this: append two AppendLists
-- together.
alistAppend :: AppendList a -> AppendList a -> AppendList a
alistAppend (AList a) (AList b) = AList (a . b)

-- Write a concatenation function which takes a list of AppendLists and turns
-- them into a single AppendList, preserving the order.
alistConcat :: [AppendList a] -> AppendList a
alistConcat = foldr alistAppend empty

-- Write a replication function which makes an AppendList by repeating the given
-- element for a given number of times (possibly zero times).
alistReplicate :: Int -> a -> AppendList a
alistReplicate n a
                   | n > 1 = alistCons a (alistReplicate (n - 1) a)
                   | n == 1 = singleton a
                   | otherwise = empty

-- Write a function that converts an append list to the regular list that it
-- represents
toList :: AppendList a -> [a]
toList (AList a) = a []

-- Write a function that does the opposite, converting a normal list into an
-- AppendList
fromList :: [a] -> AppendList a
fromList a = alistConcat $ map singleton a

-- Write a function that computes the head of an AppendList (your function may
-- fail when the AppendList is empty).
alistHead :: AppendList a -> a
alistHead a = head $ toList a

-- Write a function that computes the tail of an AppendList.
alistTail :: AppendList a -> AppendList a
alistTail = fromList . tail . toList

-- Basic tests: you should do more testing!
checkEq :: (Eq a, Show a) => a -> a -> String
checkEq expected got
    | expected == got = "PASS"
    | otherwise  = "FAIL Expected: " ++ show expected ++ " Got: " ++ show got

main :: IO ()
main = let myList  = [1, 3, 5, 7, 9] :: [Int]
           myList' = [8, 6, 4, 2, 0] :: [Int] in
       do putStr "toList/fromList: "
          putStrLn $ checkEq myList (toList . fromList $ myList)

          putStr "alistHead: "
          putStrLn $ checkEq (head myList') (alistHead . fromList $ myList')

          putStr "alistTail: "
          putStrLn $ checkEq (tail myList) (toList . alistTail . fromList $ myList)

          putStr "alistFoldr: "
          putStrLn $ checkEq (sum myList) (alistFoldr (+) 0 $ fromList myList)

          putStr "alistMap: "
          putStrLn $ checkEq (map (+ 1) myList') (toList (alistMap (+ 1) $ fromList myList'))

          putStr "alistAppend: "
          putStrLn $ checkEq (myList ++ myList') (toList $ alistAppend (fromList myList) (fromList myList'))

          putStr "alistConcat: "
          putStrLn $ checkEq (myList' ++ myList) (toList $ alistConcat [fromList myList', fromList myList])

          putStr "alistReplicate: "
          putStrLn $ checkEq ([42, 42, 42, 42, 42] :: [Int]) $ toList (alistReplicate 5 42)
