{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid reverse" #-}
import Data.List (sort, sortBy, group, nub, subsequences)
import Data.Ord (comparing)
import System.Random

-- Defining the datatypes for Deck
data Suit = Hearts 
    | Diamonds 
    | Spades 
    | Clubs
    deriving (Show, Eq, Ord, Enum, Bounded)

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

-- Extract ranks and suits from a hand
ranks :: [Card] -> [Rank]
ranks hand = [rank | (_, rank) <- hand]

suits :: [Card] -> [Suit]
suits hand = [suit | (suit, _) <- hand]

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
buildDeck = [(suit, rank) | suit <- [Hearts .. Clubs], rank <- [Two .. Ace]]

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
    | OnePair Rank [Card] -- The rank pair and the kickers e.g. OnePair King [Ten, Six, Three]
    | TwoPair Rank Rank -- The ranks of both pairs e.g. TwoPair Queen Eight
    | ThreeOfAKind Rank -- The rank of the three of a kind e.g. ThreeOfAKind Jack
    | Straight Rank -- The highest card from the straight hand e.g. Straight Nine
    | Flush [Rank] -- The ranks in decending order of the flush e.g. Flush [King, Ten, Seven, Four, Two]
    | FullHouse Rank Rank -- The ranks of the three of a kind and pair e.g. FullHouse Three Eight
    | FourOfAKind Rank -- The rank of the four of a kind e.g. FourOfAKind Five
    | StraightFlush Rank -- The highest card from the straight flush e.g. StraightFlush Eight
    | RoyalFlush
    deriving (Eq, Show, Ord)

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

-- Count occurrences of each Rank, used for checking for pairs
rankCounts :: [Card] -> [(Rank, Int)]
rankCounts hand = map (\rGroup -> (head rGroup, length rGroup)) . group . sort $ ranks hand

-- Count occurrences of each Suit, used for checking for flushes
suitCounts :: [Card] -> [(Suit, Int)]
suitCounts hand = map (\sGroup -> (head sGroup, length sGroup)) . group . sort $ suits hand

highCard :: [Card] -> HandRank
highCard hand = HighCard $ reverse . sort $ ranks hand

-- Helper function used to find pairs, triplets and quads
helperPair :: [Card] -> Int -> ([Rank], [Card])
helperPair hand a =
    let counts = rankCounts hand
        matched = [r | (r, c) <- counts, c == a]
        sorted = reverse $ sortBy (comparing snd) hand
        remaining = filter (\(_, r) -> r `notElem` matched) sorted
    in (matched, remaining)

isOnePair :: [Card] -> Maybe HandRank
isOnePair hand =
    let (pairs, remaining) = helperPair hand 2
    in case pairs of
        (p:_) -> Just (OnePair p remaining)
        _     -> Nothing

isTwoPair :: [Card] -> Maybe HandRank
isTwoPair hand =
    let (pairs, _) = helperPair hand 2
    in case pairs of
        (p1:p2:_) -> Just (TwoPair p1 p2)
        _ -> Nothing

isThreeOfAKind :: [Card] -> Maybe HandRank
isThreeOfAKind hand =
    let (trips, _) = helperPair hand 3
    in case trips of
        (t:_) -> Just (ThreeOfAKind t)
        _     -> Nothing

isStraight :: [Card] -> Maybe HandRank
isStraight hand =
    let cardRanks = ranks hand
        vs = map rankToValue cardRanks
        vsWithAceLow = if Ace `elem` cardRanks then vs ++ [1] else vs -- Add 1 to the list for a low ace in the hand
        uniqueVs = reverse . sort . nub $ vsWithAceLow
        sequences = [take 5 (drop n uniqueVs) | n <- [0..(length uniqueVs - 5)]] -- We generate every sequence of cards we can, where it is consecutive
        isConsecutive xs = and (zipWith (\a b -> a - b == 1) xs (tail xs)) -- Check if they are consecutive (straights)
        validSequences = [seq | seq <- sequences, isConsecutive seq]
    in case validSequences of
        (seq:_) -> Just (Straight (valueToRank (head seq)))
        _       -> Nothing

isFlush :: [Card] -> Maybe HandRank
isFlush hand =
    let counts = suitCounts hand
        flushSuits = [suit | (suit, count) <- counts, count >= 5] -- Find any suit that has 5 cards of the same suit
    in case flushSuits of
        (s:_) ->
            let flushCards = [rank | (suit, rank) <- hand, suit == s]
                sortedRanks = reverse . sort $ flushCards -- Sort ranks of flush
            in Just (Flush sortedRanks)
        _ -> Nothing -- If no flush, return Nothing

isFullHouse :: [Card] -> Maybe HandRank
isFullHouse hand =
    case isOnePair hand of
        Nothing -> Nothing
        Just (OnePair a xs) ->
            case isThreeOfAKind xs of
                Nothing -> Nothing
                Just (ThreeOfAKind b) -> Just (FullHouse a b)

isFourOfAKind :: [Card] -> Maybe HandRank
isFourOfAKind hand =
    let (quads, _) = helperPair hand 4
    in case quads of
        (q:_) -> Just (FourOfAKind q)
        _     -> Nothing

isStraightFlush :: [Card] -> Maybe HandRank
isStraightFlush hand =
    case isFlush hand of
        Nothing -> Nothing
        Just (Flush _) ->
            case isStraight hand of
                Nothing -> Nothing
                Just (Straight highRank) -> Just (StraightFlush highRank)

isRoyalFlush :: [Card] -> Maybe HandRank
isRoyalFlush hand =
    case isStraightFlush hand of
        Nothing -> Nothing
        Just (StraightFlush rank) ->
            if rank == Ace then Just RoyalFlush else Nothing -- Check if its a straight flush, 
                                                             -- if it's highest card is an Ace, 
                                                             -- then it is a royal flush

-- Finally, the evaluateHand function, goes through each in order of strength
evaluateHand :: [Card] -> HandRank
evaluateHand hand =
    case isRoyalFlush hand of
        Just hr -> hr
        Nothing -> case isStraightFlush hand of
            Just hr -> hr
            Nothing -> case isFourOfAKind hand of
                Just hr -> hr
                Nothing -> case isFullHouse hand of
                    Just hr -> hr
                    Nothing -> case isFlush hand of
                        Just hr -> hr
                        Nothing -> case isStraight hand of
                            Just hr -> hr
                            Nothing -> case isThreeOfAKind hand of
                                Just hr -> hr
                                Nothing -> case isTwoPair hand of
                                    Just hr -> hr
                                    Nothing -> case isOnePair hand of
                                        Just hr -> hr
                                        Nothing -> highCard hand

getPlayerBestHand :: Player -> [Card] -> HandRank
getPlayerBestHand player community =
    let cards = hand player ++ community
        fiveCardCombos = filter (\x -> length x == 5) (subsequences cards)
        handRanks = map evaluateHand fiveCardCombos
    in maximum handRanks

-- Determine the winner (given a list of player names and hand ranks)
-- Returns a list of player names
-- If there is a tie, returns multiple names
determineWinner :: [(String, HandRank)] -> [String]
determineWinner playerHandRanks
    | null playerHandRanks = []
    | otherwise =
        let bestRank = maximum (map snd playerHandRanks)
            winners = [pName | (pName, rank) <- playerHandRanks, rank == bestRank]
        in winners

main :: IO ()
main = do
    -- Step 1: Initialize the game state
    let initialPlayers = [
            Player "Alice" [] 1000 False RandomStrategy,
            Player "Bob" [] 1000 False RandomStrategy
            ]
    let initialDeck = buildDeck
    let initialGameState = GameState {
            players = initialPlayers,
            deck = initialDeck,
            communityCards = [],
            pot = 0,
            bets = [],
            dealerPosition = 0,
            smallBlindPosition = 1,
            bigBlindPosition = 2
        }

    let gameStateAfterHoleCards = dealCards DealHoleCards initialGameState
    putStrLn "After dealing hole cards:"
    putStrLn $ "Players' hands: " ++ show (map hand (players gameStateAfterHoleCards))
    putStrLn $ "Deck size: " ++ show (length (deck gameStateAfterHoleCards))

