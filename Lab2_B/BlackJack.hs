module BlackJack where
import Cards
import RunGame
import Test.QuickCheck
import System.Random

implementation = Interface
  { iFullDeck = fullDeck
  , iValue    = value
  , iDisplay  = display
  , iGameOver = gameOver
  , iWinner   = winner 
  , iDraw     = draw
  , iPlayBank = playBank
  , iShuffle  = shuffleDeck
  }


main :: IO ()
main = runGame implementation


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
valueRank Ace         = 11
valueRank _           = 10

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

-- Adds two hands together, putting the first one on top of the other hand
(<+) :: Hand -> Hand -> Hand
(<+) Empty h2     = h2
(<+) h1 Empty     = h1
(<+) (Add c h) h2 = Add c (h <+ h2)

-- Tests if (<+) is associative
prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 =
    p1<+(p2<+p3) == (p1<+p2)<+p3

-- Tests so (<+) do not change the size of the hands
prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf h1 h2 = size h1 + size h2 == size (h1<+h2) 


--Turns a list of Cards into a Hand
addCardListToHand :: [Card] -> Hand
addCardListToHand (c:cs) = Add c (addCardListToHand cs)


-- Returns an entire deck with all 52 cards

fullDeck :: Hand
fullDeck = addCardListToHand allCards 
        where ranks    = [Numeric n | n <- [2..9]] ++ [Jack,Queen,King,Ace]
              suits    = [Hearts, Spades, Clubs, Diamonds]
              allCards = [Card r s | r <- ranks, s <- suits]
        
        

--A function that draws a card from a hand/deck and returns both the hand/deck that was drawn from,
-- as well as the hand that now contains the drawn card.
draw :: Hand -> Hand -> (Hand,Hand)
draw Empty h = error "draw: The deck is empty."
draw (Add c deck) h2 = (deck, (Add c h2))

--A function that plays the round for the Bank and returns it's final hand
playBank :: Hand -> Hand
playBank deck =  playBankHelper deck Empty

--Helper function to let the bank draw card. 
playBankHelper :: Hand -> Hand -> Hand
playBankHelper deck hand 
        | value hand >= 16 = hand
        | otherwise       = playBankHelper smallerDeck biggerHand
                where (smallerDeck, biggerHand) = draw deck hand



--A function that shuffles a hand and returns the shuffled hand.
shuffleDeck :: StdGen -> Hand -> Hand
shuffleDeck g Empty = Empty
shuffleDeck g h     = shuffleDeck' g h Empty
        where shuffleDeck' g Empty h1 = h1 
              shuffleDeck' g h h1     = shuffleDeck' g' newHand (Add removedCard h1)
                        where (removedCard, newHand)  = removeNth rndNumber h
                              (rndNumber, g')         = randomR (1,size h) g
                        


-- A function that removes then N:th card of a Hand, counted from the top.
removeNth :: Integer -> Hand -> (Card, Hand)
removeNth  _ Empty     = error "Hand is empty"
removeNth  1 (Add c h) = (c , h)
removeNth  n (Add c h) = (c', Add c h')
        where (c', h') = removeNth (n - 1) h  

--A property that checks if a shuffled hand still contains the same cards
prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h =
        c `belongsTo` h == c `belongsTo` shuffleDeck g h

-- A function to check if a card belongs to a hand
belongsTo :: Card -> Hand -> Bool
c `belongsTo` Empty = False
c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h


--Proprty to test that a shuffled hand still has the same size
prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle g h = size h == size (shuffleDeck g h)


