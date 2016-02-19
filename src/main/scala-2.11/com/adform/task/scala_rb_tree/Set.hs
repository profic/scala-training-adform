
-- | Sets implemented using Red Black Trees.
--
-- Described by Chris Okasaki in the Functional Pearl:
--
-- <https://wiki.rice.edu/confluence/download/attachments/2761212/Okasaki-Red-Black.pdf "Red Black Trees in a Functional Setting">

module Data.Set.RedBlack
       (
         Tree
       , Set
       , empty
       , member
       , insert
       ) where


data Color = R | B deriving (Show, Eq)

-- |
--
-- Invariant 1: No red node has a red parent
--
-- Invariant 2: Every path from the root to an empty node contains
--              the same number of black nodes
data Tree elt = E | T Color (Tree elt) elt (Tree elt)
              deriving (Show, Eq)


-- Set based on a red black tree
type Set a = Tree a

empty :: Set elt
empty = E

member :: Ord elt => elt -> Set elt -> Bool
member _ E = False
member x (T _ a y b)
  | x <  y = member x a
  | x == y = True
  | x >  y = member x b

insert :: Ord elt => elt -> Set elt -> Set elt
insert x s = makeBlack (ins s)
  where
    ins E = T R E x E
    ins (T color a y b) | x < y  = balance color (ins a) y b
                        | x == y = T color a y b
                        | x > y  = balance color a y (ins b)
    makeBlack (T _ a y b) = T B a y b

balance :: Color -> Tree elt -> elt -> Tree elt -> Tree elt
balance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balance color a x b                 = T color a x b

balance B (T R a (T R _ _ _) x b) y (T R c z d) | B (T R a x b (T R _ _ _)) y (T R c z d)
                                                | B (T R a x b) y (T R c (T R _ _ _) z d)
                                                | B (T R a x b) y (T R c z d (T R _ _ _)) = T R (T B a x b) y (T B c z d)
-- color flip balance B (T R a (T R _ _ _) x b) y c = T B a x (T R b y c)
balance B a x (T R b y c (T R _ _ _)) = T B (T R a x b) y c
-- single rotation
balance B (T R a x (T R b y c)) z d
| B a x (T R (T R b y c) z d) = T B (T R a x b) y (T R c z d)
-- double rotation
balance color a x b = T color a x b


---- | Sets implemented using Red Black Trees.
----
---- Described by Chris Okasaki in the Functional Pearl:
----
---- <https://wiki.rice.edu/confluence/download/attachments/2761212/Okasaki-Red-Black.pdf "Red Black Trees in a Functional Setting">
--
--module Data.Set.RedBlack
--       (
--         Tree
--       , Set
--       , empty
--       , member
--       , insert
--       ) where
--
--
--data Color = Red | Black deriving (Show, Eq)
--
---- |
----
---- Invariant 1: No red node has a red parent
----
---- Invariant 2: Every path from the root to an empty node contains
----              the same number of black nodes
--data Tree T = Leaf | RBTree Color (Tree T) T (Tree T)
--              deriving (Show, Eq)
--
--
---- Set based on a red black tree
--type Set T = Tree T
--
--empty :: Set T
--empty = Leaf
--
--member :: Ord T => T -> Set T -> Bool
--member _ Leaf = False
--member x (RBTree _ a y b)
--  | x <  y = member x a
--  | x == y = True
--  | x >  y = member x b
--
--insert :: Ord T => T -> Set T -> Set T
--insert elem set = makeBlack (add set)
--  where
--    add Leaf = RBTree Red Leaf elem Leaf
--    add (RBTree color left thisVal right)
--                        | elem < thisVal  = balance color (add left) thisVal right
--                        | elem == thisVal = RBTree color left thisVal right
--                        | elem > thisVal  = balance color left thisVal (add right)
--    makeBlack (RBTree _ left thisVal right) = RBTree Black left thisVal right
--
--balance :: Color -> Tree T -> T -> Tree T -> Tree T
--balance Black (RBTree Red (RBTree Red left thisVal right) y c) z d = RBTree Red (RBTree Black left thisVal right) y (RBTree Black c z d)
--balance Black (RBTree Red left thisVal (RBTree Red right y c)) z d = RBTree Red (RBTree Black left thisVal right) y (RBTree Black c z d)
--balance Black left thisVal (RBTree Red (RBTree Red right y c) z d) = RBTree Red (RBTree Black left thisVal right) y (RBTree Black c z d)
--balance Black left thisVal (RBTree Red right y (RBTree Red c z d)) = RBTree Red (RBTree Black left thisVal right) y (RBTree Black c z d)
--balance color left thisVal right                                   = RBTree color left thisVal right