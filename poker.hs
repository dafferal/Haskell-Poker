import System.Random

-- Defining the datatypes for Deck
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
              | DealCommunityCards Int
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
        DealCommunityCards numCards ->
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
            
