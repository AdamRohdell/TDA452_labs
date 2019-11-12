module BlackJack where
import Cards
import RunGame
import Test.QuickCheck

-- A test-hand for testing our functions inputs.
hand2 = Add (Card (Numeric 2) Hearts)
            (Add (Card Jack Spades) Empty)



-- A function for checking that all steps of the size function works and gives the same response.
sizeSteps :: [Integer]
sizeSteps = [size hand2, size (Add (Card (Numeric 2) Hearts)
            (Add (Card Jack Spades) Empty)), size (Add (Card Jack Spades) Empty) + 1, size (Empty) + 2]

-- A nice way to print out a Card.
displayCard :: Card -> String
displayCard (Card (Numeric n) a) = show n ++ " of " ++ show a 
displayCard c = show (rank c) ++ " of " ++ show (suit c)

-- A nice way to print out a Hand to string (use putStr to print)
display :: Hand -> String
display Empty = ""
display (Add c h)  = displayCard c ++ "\n" ++ display h


-- Definition for the value of a Rank
valueRank :: Rank  -> Integer
valueRank (Numeric n) = n
valueRank _           = 11

--Calculates the value of a hand recursively
initialValue :: Hand -> Integer
initialValue Empty = 0
initialValue (Add c h) = valueRank (rank c) + initialValue h


--Calculates the number of Aces in a hand
numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add c h) 
        | rank c == Ace = 1 + numberOfAces h
        | otherwise = numberOfAces h

--Calculates the value of a hand for aceValue 11 and, if necessary, using aceValue 1.
value :: Hand -> Integer
value h 
    | initialValue h > 21 = (initialValue h) - (numberOfAces h) * 10
    | otherwise = initialValue h

-- Calculates if a hand busts if it's value is over 21.
gameOver :: Hand -> Bool
gameOver h 
        | value h > 21 = True
        | otherwise    = False

-- Calculates the winner using the gameOver and value functions.
winner :: Hand -> Hand -> Player
winner gh bh 
        | gameOver gh = Bank
        | gameOver bh = Guest
        | value bh >= value gh = Bank
        | otherwise = Guest
