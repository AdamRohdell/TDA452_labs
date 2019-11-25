module Sudoku where

import Test.QuickCheck

------------------------------------------------------------------------------

-- | Representation of sudoku puzzles (allows some junk)
type Cell = Maybe Int -- a single cell
type Row  = [Cell]    -- a row is a list of cells

data Sudoku = Sudoku [Row] 
 deriving ( Show, Eq )

rows :: Sudoku -> [Row]
rows (Sudoku ms) = ms

-- | A sample sudoku puzzle
example :: Sudoku
example =
    Sudoku
      [ [j 3,j 6,n  ,n  ,j 7,j 1,j 2,n  ,n  ]
      , [n  ,j 5,n  ,n  ,n  ,n  ,j 1,j 8,n  ]
      , [n  ,n  ,j 9,j 2,n  ,j 4,j 7,n  ,n  ]
      , [n  ,n  ,n  ,n  ,j 1,j 3,n  ,j 2,j 8]
      , [j 4,n  ,n  ,j 5,n  ,j 2,n  ,n  ,j 9]
      , [j 2,j 7,n  ,j 4,j 6,n  ,n  ,n  ,n  ]
      , [n  ,n  ,j 5,j 3,n  ,j 8,j 9,n  ,n  ]
      , [n  ,j 8,j 3,n  ,n  ,n  ,n  ,j 6,n  ]
      , [n  ,n  ,j 7,j 6,j 9,n  ,n  ,j 4,j 3]
      ]
  where
    n = Nothing
    j = Just

example2 :: Sudoku
example2 =
    Sudoku
      [ [j 3,j 6,j 2,j 2,j 7,j 1,j 2,j 3,j 3]
      , [j 5,j 5,j 2,j 2,j 1,j 2,j 1,j 8,j 3]
      , [j 5,j 6,j 9,j 2,j 1,j 4,j 7,j 3,j 4]
      , [j 5,j 7,j 1,j 1,j 1,j 3,j 3,j 2,j 8]
      , [j 4,j 8,j 1,j 5,j 1,j 2,j 3,j 3,j 9]
      , [j 2,j 7,j 2,j 4,j 6,j 9,j 9,j 7,j 5]
      , [j 5,j 9,j 5,j 3,j 1,j 8,j 9,j 7,j 5]
      , [j 5,j 8,j 3,j 1,j 1,j 7,j 8,j 6,j 5]
      , [j 5,j 5,j 7,j 6,j 9,j 5,j 5,j 4,j 3]
      ]
  where
    n = Nothing
    j = Just

-- * A1

-- | allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku rs
      where rs = [cs, cs, cs, cs, cs, cs, cs, cs, cs]
            cs = [n, n, n, n, n, n, n, n, n]
            n  = Nothing

-- * A2

-- | isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
removeMaybe :: Maybe Int -> Int
removeMaybe Nothing = 0
removeMaybe (Just i)  = i
--Allting jag skrev fungerar va?
isValidNumber :: Int -> Bool
isValidNumber i
          | i > 9     = False
          | i < 0     = False
          | otherwise = True

validCellsOfRow :: [Cell] -> [Bool]
validCellsOfRow []     = [True]
validCellsOfRow (c:cs) = isValidNumber (removeMaybe c) : validCellsOfRow cs


validRowsOfSudoku :: [Row] -> [Bool]
validRowsOfSudoku []  = [True]
validRowsOfSudoku (r:rs) = and (validCellsOfRow r) : validRowsOfSudoku rs

isSudoku :: Sudoku -> Bool
isSudoku sud = and [not(rows sud == []), 
                    length[ length cs == 9 |cs <- rows sud] == 9,
                      and $ validRowsOfSudoku (rows sud)]                  

-- * A3

-- | isFilled sud checks if sud is completely filled in,
-- i.e. there are no blanks
isFilled :: Sudoku -> Bool
isFilled sud = all checkEmpty (rows sud)
      where 
            checkEmpty cells =  and $ map (\val -> (removeMaybe val) <= 9 && (removeMaybe val) >= 1) cells

--isFilled sud = [i == Nothing| i <- [cs | cs <- rows sud]]

------------------------------------------------------------------------------

-- * B1

-- | printSudoku sud prints a nice representation of the sudoku sud on
-- the screen
printSudoku :: Sudoku -> IO ()
printSudoku sud =  putStr (printCell (rows sud))
        where

      

printCell :: Maybe Int -> IO ()
printCell [] = putStrLn "\n"
printCell (c:cs)
      | c == Nothing = putStrLn "." ++ printCell cs
      | otherwise = putStrLn (show(removeMaybe c)) ++ printCell cs
kolla på min lösning för isFilled
Där gör jag en lambda funktion för alla värden i en row ([Cell])
Vi vill nog göra samma här
blir det samma när det är io dock... 
Ja för det är ej rekursion, så borde fungera!
mmm
          -- * B2

-- | readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku = undefined

------------------------------------------------------------------------------

-- * C1

-- | cell generates an arbitrary cell in a Sudoku
cell :: Gen (Cell)
cell = undefined


-- * C2

-- | an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary = undefined

 -- hint: get to know the QuickCheck function vectorOf
 
-- * C3

prop_Sudoku :: Sudoku -> Bool
prop_Sudoku = undefined
  -- hint: this definition is simple!
  
------------------------------------------------------------------------------

type Block = [Cell] -- a Row is also a Cell


-- * D1

isOkayBlock :: Block -> Bool
isOkayBlock = undefined


-- * D2

blocks :: Sudoku -> [Block]
blocks = undefined

prop_blocks_lengths :: Sudoku -> Bool
prop_blocks_lengths = undefined

-- * D3

isOkay :: Sudoku -> Bool
isOkay = undefined


---- Part A ends here --------------------------------------------------------
------------------------------------------------------------------------------
---- Part B starts here ------------------------------------------------------


-- | Positions are pairs (row,column),
-- (0,0) is top left corner, (8,8) is bottom left corner
type Pos = (Int,Int)

-- * E1

blanks :: Sudoku -> [Pos]
blanks = undefined

--prop_blanks_allBlanks :: ...
--prop_blanks_allBlanks =


-- * E2

(!!=) :: [a] -> (Int,a) -> [a]
xs !!= (i,y) = undefined

--prop_bangBangEquals_correct :: ...
--prop_bangBangEquals_correct =


-- * E3

update :: Sudoku -> Pos -> Cell -> Sudoku
update = undefined

--prop_update_updated :: ...
--prop_update_updated =


------------------------------------------------------------------------------

-- * F1


-- * F2


-- * F3


-- * F4
