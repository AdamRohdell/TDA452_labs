module Sudoku where

import Test.QuickCheck
import Data.Char
import Data.List

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

example3 :: Sudoku
example3   =
    Sudoku
        [ [j 3  ,n  ,n  ,n  ,n  ,n  ,n  ,n  ,n  ]
        , [n  ,n  ,n  ,n  ,n  ,n  ,n  ,n  ,n  ]
        , [n  ,n  ,n  ,n  ,n  ,n  ,n  ,n  ,n  ]
        , [n  ,n  ,n  ,n  ,n  ,n  ,n  ,n  ,n  ]
        , [n  ,n  ,n  ,n  ,n  ,n  ,n  ,n  ,n  ]
        , [n  ,n  ,n  ,n  ,n  ,n  ,n  ,n  ,n  ]
        , [n  ,n  ,n  ,n  ,n  ,n  ,n  ,n  ,n  ]
        , [n  ,n  ,n  ,n  ,n  ,n  ,n  ,n  ,n  ]
        , [n  ,n  ,n  ,n  ,n  ,n  ,n  ,n  ,j 2]
        ]
  where
    n = Nothing
    j = Just    

exampleNotSudoku :: Sudoku
exampleNotSudoku   =
        Sudoku
            [ [j 3  ,n  ,n  ,n  ,n  ,n  ,n  ,n  ,n  ]
            , [n  ,n  ,n  ,n  ,n  ,n  ,n  ,n  ,n  ]
            , [n  ,n  ,n  ,n  ,n  ,n  ,n  ,n  ,n  ]
            , [n  ,n  ,n  ,n  ,n  ,n  ,n  ,n  ,n  ]
            , [n  ,n  ,n  ,n  ,n  ,n  ,n  ,n  ,n  ]
            , [n  ,n  ,n  ,n  ,n  ,n  ,n  ,n  ,n  ]
            , [n  ,n  ,n  ,n  ,n  ,n  ,n  ,n  ,n  ]
            , [n  ,n  ,n  ,n  ,n  ,n  ,n  ,n  ,n  ]
            , [n  ,n  ,n  ,n  ,n  ,n  ,n  ,n ]
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

--Returns the actual value of a Maybe Int as either 0 or the Int
removeMaybe :: Maybe Int -> Int
removeMaybe Nothing = 0
removeMaybe (Just i)  = i

--Checks if a number is a valid number to use in a Sudoku
isValidNumber :: Int -> Bool
isValidNumber i
          | i > 9     = False
          | i < 0     = False
          | otherwise = True

--Checks if the cells in a Sudoku row are valid
validCellsOfRow :: [Cell] -> [Bool]
validCellsOfRow []     = [True]
validCellsOfRow (c:cs) = isValidNumber (removeMaybe c) : validCellsOfRow cs

--Checks if rows of a Sudoku is valid
validRowsOfSudoku :: [Row] -> [Bool]
validRowsOfSudoku []  = [True]
validRowsOfSudoku (r:rs) = and (validCellsOfRow r) : validRowsOfSudoku rs

--isSudoku checks if a Sudoku is a valid sudoku according to standard sudoku-rules
isSudoku :: Sudoku -> Bool
isSudoku sud = and [not(rows sud == []), 
                    and ([length cs == 9 |cs <- rows sud]),
                    length[ length cs == 9 |cs <- rows sud] == 9,
                    and $ validRowsOfSudoku (rows sud)]                  

-- * A3
--and ([length cs == 9 |cs <- rows sud]),

-- | isFilled sud checks if sud is completely filled in,
-- i.e. there are no blanks
isFilled :: Sudoku -> Bool
isFilled sud = all checkEmpty (rows sud)
      where 
            checkEmpty cells =  all (\val -> val /= Nothing) cells

------------------------------------------------------------------------------

-- * B1

-- | printSudoku sud prints a nice representation of the sudoku sud on
-- the screen


printSudoku :: Sudoku -> IO ()
printSudoku sud =  putStr (printRows (rows sud))
        where   
            printRows :: [Row] -> String
            printRows []     = ""
            printRows (r:rs) = printCells r ++ printRows rs

            printCells :: [Cell] -> String
            printCells []     = "\n"
            printCells (c:cs) = printCell c ++ printCells cs

            printCell :: Cell -> String
            printCell n 
                  | n == Nothing = "."
                  | otherwise    = show (removeMaybe n) 

      


-- * B2

-- | readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku filePath = do
                s <- readFile filePath
                isSudoku' $ (Sudoku (parseSudoku s))
                where
                  parseSudoku s = parseRows $ lines s
                  parseRows :: [[Char]] -> [Row]                  
                  parseRows []   = []
                  parseRows (r:rs)  = parseCells r : parseRows rs
                  
                  parseCells :: [Char] -> [Maybe Int]
                  parseCells []  = []
                  parseCells (c:cs) = parseCell c : parseCells cs
                  
                  parseCell :: Char -> Maybe Int 
                  parseCell c 
                        | c == '.'  = Nothing
                        | otherwise = Just (digitToInt $ c)
                  isSudoku' :: Sudoku -> IO Sudoku
                  isSudoku' sud 
                            | isSudoku sud = return sud
                            | otherwise    = error "Not a Sudoku!"
                

                 
      
        
        



------------------------------------------------------------------------------

-- * C1

-- | cell generates an arbitrary cell in a Sudoku
cell :: Gen (Cell)
cell = frequency[(9, return Nothing), (1, do 
                                            n <- choose(1,9)
                                            return (Just n))]
      


-- * C2

-- | an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary = do
                rs <- (vectorOf 9 (vectorOf 9 cell))
                return $ Sudoku rs

 -- hint: get to know the QuickCheck function vectorOf
 
-- * C3
--Property for testing if a Sudoku is a valid Sudoku
prop_Sudoku :: Sudoku -> Bool
prop_Sudoku sud = isSudoku sud
  -- hint: this definition is simple!
  
------------------------------------------------------------------------------

type Block = [Cell] -- a Row is also a [Cell]


-- * D1
--A method to test if a block is following all the block-rules
isOkayBlock :: Block -> Bool
isOkayBlock []     = True
isOkayBlock (c:cs) 
          | c == Nothing = True && isOkayBlock cs 
          | otherwise    = notElem c cs && isOkayBlock cs 


-- * D2
blocks :: Sudoku -> [Block]
blocks(Sudoku rs) = rs ++ collumnsFromRows rs ++ blocksFromRows rs
      where
        collumnsFromRows ms        = transpose ms

        blocksFromRows ls          = firstBlocks ls ++ secondBlocks ls ++ thirdBlocks ls

        firstBlocks []             = []
        firstBlocks  (r1:r2:r3:rs) = (getFirstOfTripple r1 ++ getFirstOfTripple r2 ++ getFirstOfTripple r3) : firstBlocks rs
        
        secondBlocks []            = []
        secondBlocks (r1:r2:r3:rs) = (getSecondOfTripple r1 ++ getSecondOfTripple r2 ++ getSecondOfTripple r3) : secondBlocks rs
      
        thirdBlocks []             = []
        thirdBlocks (r1:r2:r3:rs)  = (getThirdOfTripple r1 ++ getThirdOfTripple r2 ++ getThirdOfTripple r3) : thirdBlocks rs
        
        getFirstOfTripple r        = take 3 r

        getSecondOfTripple r       = take 3 (drop 3 r)

        getThirdOfTripple r        = drop 6 r
            

--Property to test the lengths of the blocks in a Sudoku
prop_blocks_lengths :: Sudoku -> Bool
prop_blocks_lengths sud = length([c | c <- blocks sud]) == 27 && 
                          and ([length c == 9 | c <- blocks sud])

-- * D3

--A method to test if each block in a Sudoku is okay
isOkay :: Sudoku -> Bool
isOkay sud              = isOkay' (blocks sud)
    where
        isOkay' []      = True
        isOkay' (b:bl)  = isOkayBlock b && isOkay' bl


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
