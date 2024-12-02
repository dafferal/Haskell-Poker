import Data.List (sort, group, sortBy, nub)
import Data.Ord (comparing)
import Data.Function (on)
import System.Random

-- Defining the datatypes for Deck
data Suit = Hearts
    | Diamonds
    | Spades
    | Clubs
    deriving (Eq, Ord, Show)

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
    | Ace
    deriving (Show, Eq, Ord, Enum, Bounded)

type Card = (Suit, Rank)

type Deck = [Card]

-- Defining the Player data type
data Strategy = RandomStrategy
              | PlaceholderStrategy
              deriving (Show)

data Player = Player {
    playerName :: String,
    hand       :: [Card],
    chips      :: Int,
    isDealer   :: Bool,
    strategy   :: Strategy
} deriving (Show)

-- Defining the GameState data type
data GameState = GameState {
    players            :: [Player],
    deck               :: Deck,
    communityCards     :: [Card],
    pot                :: Int,
    bets               :: [(String, Int)], -- Each bet will be linked to a specific Player through their name
    dealerPosition     :: Int,
    smallBlindPosition :: Int,
    bigBlindPosition   :: Int
} deriving (Show)

buildDeck :: Deck
buildDeck = [(suit, rank) | suit <- suitList, rank <- [Two .. King]]
  where
    suitList = [Hearts, Diamonds, Spades, Clubs]

shuffleDeck :: Deck -> IO Deck
shuffleDeck [] = return []
shuffleDeck xs = do
    randIndex <- randomRIO (0, length xs - 1)
    let selectedCard = xs !! randIndex
        remainingDeck = take randIndex xs ++ drop (randIndex + 1) xs -- Selecting random index within given Deck
    shuffledDeck <- shuffleDeck remainingDeck -- Recursively shuffle deck
    return (selectedCard : shuffledDeck)

-- Functions to deal hole and community cards
data DealType = DealHoleCards
              | DealCommunityCards Int -- Number of cards to be dealt
              deriving (Show)

dealCards :: DealType -> GameState -> GameState
dealCards dealType gameState =
    case dealType of
        DealHoleCards ->
            let
                currentDeck = deck gameState
                currentPlayers = players gameState
                (updatedPlayers, newDeck) = dealToPlayers currentPlayers currentDeck
            in
                gameState { players = updatedPlayers, deck = newDeck }
        DealCommunityCards numCards -> -- Deals cards depending on input
            let
                currentDeck = deck gameState
                currentCommunityCards = communityCards gameState
                (cardsDealt, newDeck) = splitAt numCards currentDeck
                updatedCommunityCards = currentCommunityCards ++ cardsDealt
            in
                gameState { communityCards = updatedCommunityCards, deck = newDeck }

-- Helper function to deal two cards to each player
dealToPlayers :: [Player] -> Deck -> ([Player], Deck) 
dealToPlayers [] deck = ([], deck)
dealToPlayers (player:restPlayers) deck =
    let
        (cardsDealt, deckAfterDealing) = splitAt 2 deck
        updatedPlayer = player { hand = hand player ++ cardsDealt }
        (updatedRestPlayers, finalDeck) = dealToPlayers restPlayers deckAfterDealing
    in
        (updatedPlayer : updatedRestPlayers, finalDeck) -- Returns the updated players and deck
            
data HandRank -- Evaluating the rank of a hand
    = HighCard [Rank] -- The ranks in descending order
    | OnePair Rank [Rank] -- The rank pair and the kickers e.g. OnePair King [Ten, Six, Three]
    | TwoPair Rank Rank Rank -- The ranks of both pairs and the kicker e.g. TwoPair Queen Eight Two
    | ThreeOfAKind Rank [Rank] -- The rank of the three of a king and the kicker e.g. ThreeOfAKind Jack [Nine, Five]
    | Straight Rank -- The highest card from the straight hand e.g. Straight Nine
    | Flush [Rank] -- The ranks in decending order of the flush e.g. Flush [King, Ten, Seven, Four, Two]
    | FullHouse Rank Rank -- The ranks of the three of a kind and pair e.g. FullHouse Three Eight
    | FourOfAKind Rank Rank -- The ranks of the four of a kind and kicker e.g. FourOfAKind Five King
    | StraightFlush Rank -- The highest card from the straight flush e.g. StraightFlush Eight
    | RoyalFlush
    deriving (Eq, Show, Ord)

-- Extract ranks and suits from a hand
ranks :: [Card] -> [Rank]
ranks hand = [rank | (_, rank) <- hand]

suits :: [Card] -> [Suit]
suits hand = [suit | (suit, _) <- hand]

-- Convert Rank to numeric value for straights
rankToValue :: Rank -> Int
rankToValue r = case r of
    Two   -> 2
    Three -> 3
    Four  -> 4
    Five  -> 5
    Six   -> 6
    Seven -> 7
    Eight -> 8
    Nine  -> 9
    Ten   -> 10
    Jack  -> 11
    Queen -> 12
    King  -> 13
    Ace   -> 14

-- Convert numeric value back to Rank
valueToRank :: Int -> Rank
valueToRank v = case v of
    2  -> Two
    3  -> Three
    4  -> Four
    5  -> Five
    6  -> Six
    7  -> Seven
    8  -> Eight
    9  -> Nine
    10 -> Ten
    11 -> Jack
    12 -> Queen
    13 -> King
    14 -> Ace
    _  -> error "Not a rank"

-- Count occurrences of each Rank, used for checking for pairs
rankCounts :: [Card] -> [(Rank, Int)]
rankCounts hand = map (\rGroup -> (head rGroup, length rGroup)) . group . sort $ ranks hand

-- Count occurrences of each Suit, used for checking for flushes
suitCounts :: [Card] -> [(Suit, Int)]
suitCounts hand = map (\sGroup -> (head sGroup, length sGroup)) . group . sort $ suits hand

-- For evaluating the hands 

highCard :: [Card] -> HandRank
highCard hand = HighCard $ reverse . sort $ ranks hand

isOnePair :: [Card] -> Maybe HandRank
isOnePair hand =
    let counts = rankCounts hand
        pairs = [rank | (rank, count) <- counts, count == 2]
        remaining = reverse . sort $ [rank | (rank, count) <- counts, count /= 2]
    in case pairs of
        (p:_) -> Just (OnePair p remaining)
        _     -> Nothing

