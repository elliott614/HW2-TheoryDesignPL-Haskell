-- CS 839, Spring 2019: Homework 2
-- Part 3: Tree zippers (40)

-- Do not change the following line!
{-# OPTIONS -fwarn-incomplete-patterns -fwarn-tabs -fno-warn-type-defaults #-}

-- **Your code should compile without warnings.**
-- The following line makes the compiler treat all warnings as hard errors.
-- When you are done, uncomment it and fix until there are no more errors.
-- (You may ignore unused function warnings.)
{-# OPTIONS -Wall -fno-warn-unused-local-binds -Werror -Wno-error=unused-local-binds #-}

-- You might want some functions from these libraries:
-- Data.List

-- We'll now extend zippers to work for trees. First, we'll set up a datatype
-- for plain trees. Trees will either consist of a leaf with a piece of data of
-- type `a`, or a list of trees. Complete the following datatype declaration,
-- replacing () with the correct types.

data Tree a =
    Leaf a
  | Node [Tree a]

-- Just like before, we want to represent a (1) position and an (2) item with
-- our zipper. Both of these pieces are more complicated for trees. Let's start
-- with representing positions in a tree. Positions may be either leaf nodes or
-- internal nodes. We will represent positions recursively: a position of a node
-- is either the root of a tree, or it is the position of its parent node along
-- with two lists describing the node's siblings (child nodes of the same
-- parent): the sibling trees before the current node, and sibling trees after
-- the current node.
--
-- Fill out the following datatype declaration for the type of paths, replacing
-- (or removing) ().

data Path a =
    Top
  | Loc [Tree a] (Path a) [Tree a]

-- Just like we did for list zippers, we will store the list of siblings before
-- the current position in *reverse* order, and the list of siblings after the
-- current position in *normal* order. Keep this invariant in mind and make sure
-- that it is preserved in all the operations.

-- Since the position may be an internal node, the item at a position may be a
-- tree. By combining these two pieces of data, we are ready to define the type
-- of tree zippers.

data TreeZipper a = TZ { getPath :: Path a, getItem :: Tree a }

-- Write a function to convert a regular tree into a zipper, with the initial
-- position set to the root.

zipOfTree :: Tree a -> TreeZipper a
zipOfTree a = TZ { getPath = Top, getItem = a }

-- Write a function to convert a zipper back into a regular tree.
-- (Hint: you'll want to recurse, calling zipToTree on a smaller path.)

zipToTree :: TreeZipper a -> Tree a
zipToTree TZ { getPath = Top, getItem = i } = i
zipToTree TZ { getPath = Loc l p r, getItem = i } = zipToTree TZ { getPath = p, getItem = Node (reverse l ++ [i] ++ r) }

-- Write a function to get the subtree at the current position.

getCurTree :: TreeZipper a -> Tree a
getCurTree TZ { getPath = _, getItem = a } = a

-- For trees, we can move the position in more directions. A left/right move
-- corresponds to switching to one of the siblings of the current position,
-- while an up/down move corresponds to moving to the parent or moving to the
-- first child of the current position.
--
-- If the move is not valid (trying to move left at the first sibling, or trying
-- to move right in the last sibling, etc.), return the original zipper with no
-- change. Your function should be total---it should never raise an error!

goLeftT :: TreeZipper a -> TreeZipper a
goLeftT TZ { getPath = Top, getItem = i } = TZ { getPath = Top, getItem = i }
goLeftT TZ { getPath = Loc l p r, getItem = i }
                                              | null l = TZ { getPath = Loc l p r, getItem = i }
                                              | otherwise = TZ { getPath = Loc (tail l) p (i:r), getItem = head l }

goRightT :: TreeZipper a -> TreeZipper a
goRightT TZ { getPath = Top, getItem = i } = TZ { getPath = Top, getItem = i }
goRightT TZ { getPath = Loc l p r, getItem = i }
                                              | null r = TZ { getPath = Loc l p r, getItem = i }
                                              | otherwise = TZ { getPath = Loc (i:l) p (tail r), getItem = head r }

goUpT :: TreeZipper a -> TreeZipper a
goUpT TZ { getPath = Top, getItem = i } = TZ { getPath = Top, getItem = i }
goUpT TZ { getPath = Loc l p r, getItem = i } = TZ { getPath = p, getItem = Node (reverse l ++ [i] ++ r) }

goDownT :: TreeZipper a -> TreeZipper a
goDownT TZ { getPath = p, getItem = Leaf a } = TZ { getPath = p, getItem = Leaf a}
goDownT TZ { getPath = p, getItem = Node n }
                                            | null n = TZ { getPath = p, getItem = Node n }
                                            | otherwise = TZ { getPath = Loc [] p (tail n), getItem = head n }

-- Write a function to replace the tree at the current position in the zipper
-- with a new subtree. Again, your function should never raise an error!

updateTree :: Tree a -> TreeZipper a -> TreeZipper a
updateTree t TZ { getPath = Top, getItem = _ } = TZ { getPath = Top, getItem = t }
updateTree t TZ { getPath = Loc l p r, getItem = _} = TZ { getPath = Loc l p r, getItem = t }

-- Write a function to insert a new subtree into the zipper. There are three
-- possible places to insert: left (as a new sibling before the current
-- position), right (as a new sibling after the current position), and down (as
-- a new first/left-most child of the current position). The cursor should end
-- up on the newly inserted item. If the insertion is invalid (inserting
-- left/right at the root), return the original zipper.  Again, your function
-- should never raise an error!

insertLeftT :: Tree a -> TreeZipper a -> TreeZipper a
insertLeftT _ TZ { getPath = Top, getItem = i } = TZ { getPath = Top, getItem = i }
insertLeftT t TZ { getPath = Loc l p r, getItem = i } = TZ { getPath = Loc l p (i:r), getItem = t }

insertRightT :: Tree a -> TreeZipper a -> TreeZipper a
insertRightT _ TZ { getPath = Top, getItem = i } = TZ { getPath = Top, getItem = i }
insertRightT t TZ { getPath = Loc l p r, getItem = i } = TZ { getPath = Loc (i:l) p r, getItem = t }

insertDownT :: Tree a -> TreeZipper a -> TreeZipper a
insertDownT _ TZ { getPath = p, getItem = Leaf l } = TZ { getPath = p, getItem = Leaf l }
insertDownT t TZ { getPath = p, getItem = Node n } = TZ { getPath = Loc [] p n, getItem = t}

-- Write a function to delete the whole subtree at the current position from the
-- zipper. The final position of the zipper should be (in decreasing priority)
-- the next sibling on the right, or the previous sibling on the left, or the
-- parent node if there are no other siblings.
--
-- Again, your function should behave correctly for all zippers; deleting the
-- root node should return the original zipper.

deleteT :: TreeZipper a -> TreeZipper a
deleteT TZ { getPath = Top, getItem = i } = TZ { getPath = Top, getItem = i }
deleteT TZ { getPath = Loc l p r, getItem = _}
                                              | null l && null r = TZ { getPath = p, getItem = Node (reverse l ++ r) }
                                              | null r = TZ { getPath = Loc (tail l) p r, getItem = head l }
                                              | otherwise = TZ { getPath = Loc l p (tail r), getItem = head r }

-- This main function forms lists of strings. You enter commands to move around
-- the zipper and modify the list using it. If you want to start from a
-- different zipper, change the definition below.
-- (Note: the following will not compile until you define the TreeZipper type.)

initZip :: TreeZipper Integer
initZip = TZ {getPath=l4Path, getItem=l4}
    where
        n1 = Node[n2, n3]
        n2 = Node[l1, l2]
        n3 = Node[n4, l5]
        n4 = Node[l3, l4]
        l1 = Leaf 1
        l2 = Leaf 2
        l3 = Leaf 3
        l4 = Leaf 4
        n1Path = Top
        l5 = Leaf 5

        n2Path = Loc [] n1Path [n3]
        l1Path = Loc [] n2Path [l2]
        l2Path = Loc [l1] n2Path []

        n3Path = Loc [n2] n1Path []

        n4Path = Loc [] n3Path [l5]
        l5Path = Loc [n4] n3Path []

        l3Path = Loc [] n4Path [l4]
        l4Path = Loc [l3] n4Path []

main :: IO ()
main = 
    aux initZip
    where headMay xs = case xs of
              (x:_) -> Just x
              []    -> Nothing
          isDigit c = c `elem` "0123456789"
          showTree t = case t of
              Leaf a -> show a
              Node children -> "(" ++ unwords ch_strs ++ ")"
                               where ch_strs = map showTree children
          readTree word = case word of 
                           "()" -> Right (Node [])
                           _    -> if all isDigit word
                                   then Right (Leaf (read word))
                                   else Left ("!! Error: Second argument must "
                                                       ++ "be a number or ()")
          onSnd wrds = case tail wrds of
              []      -> Left "!! Error: need second argument"
              (sec:_) -> readTree sec
          aux :: TreeZipper Integer -> IO ()
          aux zipp = do
              putStr "-- Current Tree: "
              putStr $ showTree (zipToTree zipp)
              putStr " at subtree: "
              print (showTree (getCurTree zipp))
              putStrLn ("-- Enter an operation on the list:\n" ++ 
                        "-- (l)eft, (r)ight, (u)p, (d)own,\n-- (e)dit, " ++
                        "(il) insert left, (ir) insert right, " ++
                        "(id) insert down,\n-- or (del)ete:")
              input <- getLine
              let wrds = words input
              case headMay wrds of
                  Just "l" -> aux (goLeftT zipp)
                  Just "r" -> aux (goRightT zipp)
                  Just "u" -> aux (goUpT zipp)
                  Just "d" -> aux (goDownT zipp)
                  Just "e" -> case onSnd wrds of
                                  Left msg -> putStrLn msg >> aux zipp
                                  Right tr -> aux (updateTree tr zipp)
                  Just "il" -> case onSnd wrds of
                                    Left msg -> putStrLn msg >> aux zipp
                                    Right tr -> aux (insertLeftT tr zipp)
                  Just "ir" -> case onSnd wrds of
                                    Right tr -> aux (insertRightT tr zipp)
                                    Left str -> putStrLn str >> aux zipp
                  Just "id" -> case onSnd wrds of
                                    Right tr -> aux (insertDownT tr zipp)
                                    Left str -> putStrLn str >> aux zipp
                  Just "del" -> aux (deleteT zipp)
                  _          ->    putStrLn "!! Error: unrecognized command"
                                >> aux zipp
