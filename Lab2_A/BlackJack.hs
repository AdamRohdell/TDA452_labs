module BlackJack where
import Cards
import RunGame
import Test.QuickCheck

hand2 = Add (Card (Numeric 2) Hearts)
            (Add (Card Jack Spades) Empty)



-- A function for checking that all steps of the size function works and gives the same response.
sizeSteps :: [Integer]
sizeSteps = [size hand2, size (Add (Card (Numeric 2) Hearts)
            (Add (Card Jack Spades) Empty)), size (Add (Card Jack Spades) Empty) + 1, size (Empty) + 2]


displayCard :: Card -> String

displayCard (Card (Numeric n) a) = show n ++ " of " ++ show a 
displayCard c = show (rank c) ++ " of " ++ show (suit c)



display :: Hand -> String
display Empty = ""
display (Add c h)  = displayCard c ++ "\n" ++ display h


valueRank :: Rank  -> Integer
valueRank (Numeric n) = n
valueRank _           = 11

initialValue :: Hand -> Integer
initialValue Empty = 0
initialValue (Add c h) = valueRank (rank c) + initialValue h

numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add c h) 
        | rank c == Ace = 1 + numberOfAces h
        | otherwise = numberOfAces h


value :: Hand -> Integer
value h 
    | initialValue h > 21 = (initialValue h) - (numberOfAces h) * 10
    | otherwise = initialValue h
