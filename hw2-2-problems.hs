-- CS 839, Spring 2019: Homework 2
-- Part 2: List zippers (30)

-- Do not change the following line!
{-# OPTIONS -fwarn-incomplete-patterns -fwarn-tabs -fno-warn-type-defaults #-}

-- **Your code should compile without warnings.**
-- The following line makes the compiler treat all warnings as hard errors.
-- When you are done, uncomment it and fix until there are no more errors.
-- (You may ignore unused function warnings.)
{-# OPTIONS -Wall -Werror #-}

-- You might want some functions from these libraries:
-- Data.List

-- A zipper takes a plain datastructure---a list, a tree, etc.---and adds an
-- extra bit of information describing a position within the datastructure. This
-- position behaves much like a cursor: we can move the cursor, lookup the value
-- at the cursor, or update/delete the value at the cursor. 

-- We can represent a position in a non-empty plain list with three parts: the
-- list of elements before the position, the element at the position, and the
-- list of elements after the position. Otherwise, the zipper may be over an
-- empty list. The elements of the list will be of type `a`. 

data ListZipper a =
    LZEmpty
  | LZItems [a]   -- list of items before
            a     -- current item
            [a]   -- list of items after

-- One detail about Haskell lists is that they support efficient operations at
-- only one end, the start (head) of the list. Working at the tail end
-- requires traversing the whole list, an expensive operation.
-- 
-- To make the zipper operations easier to write and more efficient, we
-- store the list of items before in *reversed* order. For instance, if the
-- underlying list is [1, 2, 3, 4, 5] and the current position is 3, then the
-- zipper should look like
--
-- LzItems [2, 1] 3 [4, 5]
--
-- Keep this invariant in mind, and make sure it is maintained through all of
-- the operations.
--
-- Write a function to convert a regular list into a zipper, with the initial
-- position set to the first element (if the list is non-empty).

zipOfList :: [a] -> ListZipper a
zipOfList [] = LZEmpty
zipOfList (x:xs) = LZItems [] x xs

-- Write a function to convert a zipper back into a regular list.

zipToList :: ListZipper a -> [a]
zipToList LZEmpty = []
zipToList (LZItems a b c) = reverse a ++ [b] ++ c

-- Write a function to get the current item. Note that the function returns
-- `Maybe a`. Your function should return `Nothing` if the zipper is empty.

getCurItem :: ListZipper a -> Maybe a
getCurItem LZEmpty = Nothing
getCurItem (LZItems _ a _) = Just a

-- Write functions to move the position left or right in the zipper. Since we
-- cannot actually change the input zipper in a pure functional language, your
-- function will return a zipper with the updated position. Remember to keep
-- track of the elements in the zipper: moving left or right should not change
-- the elements in the underlying list.
--
-- If the input zipper is empty, or the move is not valid (trying to move left
-- in the first position, or trying to move right in the last position), return
-- the original zipper with no change. Your function should be total---it should
-- never raise an error!

goLeftL :: ListZipper a -> ListZipper a
goLeftL LZEmpty = LZEmpty
goLeftL (LZItems a b c)
                        | null a = LZItems a b c
                        | otherwise = LZItems (tail a) (head a) (b:c)

goRightL :: ListZipper a -> ListZipper a
goRightL LZEmpty = LZEmpty
goRightL (LZItems a b c)
                        | null c = LZItems a b c
                        | otherwise = LZItems (b:a) (head c) (tail c)

-- Write a function to replace the item at the current position in the zipper
-- with a new element. If the input zipper is empty, return the input zipper
-- unchanged. Again, your function should never raise an error!

updateItem :: a -> ListZipper a -> ListZipper a
updateItem _ LZEmpty = LZEmpty
updateItem d (LZItems a _ c) = LZItems a d c

-- Write a function to insert a new element into the zipper *after* the current
-- position. The cursor should move to the new item after the insertion. Your
-- function should behave correctly for all zippers, including the empty zipper.

insertItem :: a -> ListZipper a -> ListZipper a
insertItem a LZEmpty = zipOfList [a]
insertItem d (LZItems a b c) = LZItems (b:a) d c

-- Write a function to delete the current element from the zipper. The cursor
-- should move to the item before the deleted position, or after the first
-- position when deleting the first item. Again, your function should behave
-- correctly for all zippers, including the empty zipper. (Deleting from the
-- empty zipper should just return the empty zipper.)

deleteItem :: ListZipper a -> ListZipper a
deleteItem LZEmpty = LZEmpty
deleteItem (LZItems a _ c)
                          | null a && null c = LZEmpty
                          | null a = LZItems a (head c) (tail c)
                          | otherwise = LZItems (tail a) (head a) c

-- This main function forms lists of strings. You enter commands to move around
-- the zipper and modify the list using it. If you want to start from a
-- different zipper, change the definition below.

initZip :: ListZipper String
initZip = zipOfList []

main :: IO ()
main = 
    aux initZip
    where headMay xs = case xs of
              (x:_) -> Just x
              []    -> Nothing
          isSpace c = c `elem` "\t \n\r\v\f"
          aux zipp = do
              putStr "-- Current list: "
              putStr $ show (zipToList zipp)
              case getCurItem zipp of
                   Just el -> do putStr " at element: "
                                 print el
                   Nothing -> putStrLn ""
              putStrLn ("-- Enter an operation on the list: " ++ 
                        "(l)eft, (r)ight, (e)dit, (i)nsert, or (d)elete:")
              input <- getLine
              case headMay input of
                  Just 'l' -> aux (goLeftL zipp)
                  Just 'r' -> aux (goRightL zipp)
                  Just 'e' -> let w2 = dropWhile isSpace (tail input)
                              in aux (updateItem w2 zipp)
                  Just 'i' -> let w2 = dropWhile isSpace (tail input)
                              in aux (insertItem w2 zipp)
                  Just 'd' -> aux (deleteItem zipp)
                  _ -> do putStrLn "!! Error: unrecognized command"
                          aux zipp

