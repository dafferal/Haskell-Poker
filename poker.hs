import System.Random

-- Building the datatypes for the deck
data Suit = Hearts
    | Diamonds
    | Spades
    | Clubs
    deriving (Show)

data Rank = Two
    | Three
    | Four 
    | Five
    | Six
    | Seven 
    | Eight 
    | Nine
    | Ten 
    | Jack 
    | Queen
    | King
    deriving (Show, Eq, Ord, Enum)

type Card = (Suit, Rank)

type Deck = [Card]

buildDeck :: Deck
buildDeck = [(suit, rank) | suit <- suitList, rank <- [Two .. King]]
  where
    suitList = [Hearts, Diamonds, Spades, Clubs]