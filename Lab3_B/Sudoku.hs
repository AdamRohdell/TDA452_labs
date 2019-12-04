module Sudoku where

import Test.QuickCheck
import Data.Char
import Data.List
import Data.Maybe

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
                    and ([length cs == 9 |cs <- rows sud]),
                    length[ length cs == 9 |cs <- rows sud] == 9,
                    and $ validRowsOfSudoku (rows sud)]                  

-- * A3

-- | isFilled sud checks if sud is completely filled in,
-- i.e. there are no blanks
isFilled :: Sudoku -> Bool
isFilled sud = all checkEmpty (rows sud)
      where 
            checkEmpty cells =  all (\val -> val /= Nothing) cells

--isFilled sud = [i == Nothing| i <- [cs | cs <- rows sud]]

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

prop_Sudoku :: Sudoku -> Bool
prop_Sudoku sud = isSudoku sud
  -- hint: this definition is simple!
  
------------------------------------------------------------------------------

type Block = [Cell] -- a Row is also a [Cell]


-- * D1
isOkayBlock :: Block -> Bool
isOkayBlock []     = True
isOkayBlock (c:cs) 
          | c == Nothing = True && isOkayBlock cs 
          | otherwise    = notElem c cs && isOkayBlock cs 


-- * D2
blocks :: Sudoku -> [Block]
blocks(Sudoku rs) = rs ++ collumnsFromRows rs ++ blocksFromRows rs
      where
        collumnsFromRows ms        =   transpose ms

        blocksFromRows ls          =  firstBlocks ls ++ secondBlocks ls ++ thirdBlocks ls

        firstBlocks []             = []
        firstBlocks  (r1:r2:r3:rs) = (getFirstOfTripple r1 ++ getFirstOfTripple r2 ++ getFirstOfTripple r3) : firstBlocks rs
        
        secondBlocks []            = []
        secondBlocks (r1:r2:r3:rs) =   (getSecondOfTripple r1 ++ getSecondOfTripple r2 ++ getSecondOfTripple r3) : secondBlocks rs
        
        thirdBlocks []             = []
        thirdBlocks (r1:r2:r3:rs)  =   (getThirdOfTripple r1 ++ getThirdOfTripple r2 ++ getThirdOfTripple r3) : thirdBlocks rs
        
        getFirstOfTripple r        =   take 3 r

        getSecondOfTripple r       =   take 3 (drop 3 r)

        getThirdOfTripple r        =   drop 6 r
            

prop_blocks_lengths :: Sudoku -> Bool
prop_blocks_lengths sud = length([c | c <- blocks sud]) == 27 && 
                          and ([length c == 9 | c <- blocks sud])
-- * D3

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
blanks (Sudoku rs) = blanks' rs 0 
    where
      blanks' :: [Row] -> Int -> [Pos]
      blanks' (r:rs) x     
                  | x == 8    = checkIfRowBlanks r x 0
                  | otherwise = checkIfRowBlanks r x 0 ++ blanks' rs (x+1)   
      
      checkIfRowBlanks (c:cs) x y 
                            | y == 8  && c == Nothing = (x,y) : []
                            | y == 8                  = []
                            | c == Nothing            = (x,y) : checkIfRowBlanks cs x (y+1)
                            | otherwise               = checkIfRowBlanks cs x (y+1)
      
      
      
prop_blanks_allBlanks :: Sudoku -> Bool
prop_blanks_allBlanks sud = undefined --length (filter (\val -> val == Nothing) (rows sud)) == length (blanks sud)

--Vänta med denna propertyn tills vi har skrivit lite mer kod på nästa uppgift


-- * E2

(!!=) :: [a] -> (Int,a) -> [a]
[] !!= (i,y)            = []
xs !!= (i,y) 
        | i < 0         = error "Negative index"
        | i < length xs = indexHelper xs (i,y) 0
        | otherwise     = error "Index out of bounds"
      where indexHelper (a:as) (j,v) n
                              | n == j    = (v:as)
                              | otherwise = (a:(indexHelper as (j,v) (n+1)))

prop_bangBangEquals_correct :: [Cell] -> (Int, Cell) -> Bool
prop_bangBangEquals_correct [] (i,y) = [] !!= (abs i,y) == []
prop_bangBangEquals_correct xs (i,y) =
                    length newList == length xs &&
                    last (take (j+1) newList) == y
                      where 
                        j 
                          | abs i > (length xs -1) = (abs i) `mod` (length xs)
                          | otherwise         = abs i 
                        newList = xs !!= (j,y)
      


-- * E3

update :: Sudoku -> Pos -> Cell -> Sudoku
update (Sudoku rs) (x,y) c 
          | x > 8 || x < 0 = error "Index out of bounds"
          | otherwise      = Sudoku (update' rs 0)
              where 
                update' (r:rs) i 
                        | x == i    = (r !!= (y, c):rs)
                        | otherwise =  (r:update' rs (i+1))



                  

prop_update_updated :: Sudoku -> Pos -> Cell -> Bool
prop_update_updated sud (x,y) c = cellAtIndex == c
          where
            rs          = rows (update sud (x',y') c)
            rowAtIndex  = last (take (x'+1) rs)
            cellAtIndex = last (take (y'+1) rowAtIndex)
            x' 
              | abs x < length (rows sud) = abs x
              | otherwise                 = (abs x) `mod` length (rows sud)
            y'
              | abs y < length (rows sud) = abs y
              | otherwise                 = (abs y) `mod` length (rows sud)


------------------------------------------------------------------------------

-- * F1
solve :: Sudoku -> Maybe Sudoku
solve sud
          | null (solve' sud) = Nothing
          | otherwise         = Just $ head (solve' sud)
              where 
                solve' s 
                        | not (isSudoku s) || not (isOkay s) = []
                        | isFilled s = [s]
                        | otherwise = solve'' s (blanks s)
                solve'' sud' (p':ps) = concat $ map solve' [update sud' p' (Just c) | c <- [1..9]]



-- * F2
readAndSolve :: FilePath -> IO ()
readAndSolve fp =do
       sud <- readSudoku fp
       printSudoku(fromJust(solve sud))
-- * F3
isSolutionOf :: Sudoku -> Sudoku -> Bool
sud1 `isSolutionOf` sud2 
        | not(isFilled sud1 && isOkay sud1) = False
        | otherwise                         = checkZips zips


          where
            zips = zip (concat $ rows sud1) (concat $ rows sud2)
            checkZips []                 = True
            checkZips ((x,y):xys)
                | y == Nothing || x == y = True && checkZips xys
                | otherwise              = False
          
-- * F4
prop_SolveSound :: Sudoku -> Property
prop_SolveSound sud = isOkay sud ==> fromJust (solve sud) `isSolutionOf` sud

fewerChecks prop =
  quickCheckWith stdArgs{maxSuccess=30} prop