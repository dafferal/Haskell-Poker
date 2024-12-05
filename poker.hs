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
        pairs = [rank | (rank, count) <- counts, count == 2] -- Check for pair
        remaining = reverse . sort $ [rank | (rank, count) <- counts, count /= 2] -- Sort kickers
    in case pairs of
        (p:_) -> Just (OnePair p remaining) -- If there is a pair, return OnePair
        _     -> Nothing -- Else return Nothing

isTwoPair :: [Card] -> Maybe HandRank
isTwoPair hand =
    let counts = rankCounts hand
        pairs = reverse . sort $ [rank | (rank, count) <- counts, count == 2] -- Check for two pairs
        remaining = reverse . sort $ [rank | (rank, count) <- counts, count == 1] -- Sort kickers
    in case pairs of
        (p1:p2:_) ->
            let kicker = if not (null remaining) then head remaining else Two
            in Just (TwoPair p1 p2 kicker) -- If there is two pairs, return TwoPair
        _ -> Nothing -- If there are no pairs, return Nothing

--Same as isTwoPair, except for three of a kind
isThreeOfAKind :: [Card] -> Maybe HandRank
isThreeOfAKind hand =
    let counts = rankCounts hand
        triplets = [rank | (rank, count) <- counts, count == 3]
        remaining = reverse . sort $ [rank | (rank, count) <- counts, count /= 3]
    in case triplets of
        (r:_) -> Just (ThreeOfAKind r remaining)
        _     -> Nothing

isStraight :: [Card] -> Maybe HandRank
isStraight hand =
    case straightValue hand of
        Just highRank -> Just (Straight highRank)
        Nothing -> Nothing

-- Helper function to determine straights
straightValue :: [Card] -> Maybe Rank
straightValue hand =
    let vs = map rankToValue (ranks hand)
        vsWithAceLow = if Ace `elem` ranks hand then vs ++ [1] else vs -- If there is an Ace, add 1 to the list too
        uniqueVs = reverse . sort . nub $ vsWithAceLow
        sequences = [take 5 (drop n uniqueVs) | n <- [0..(length uniqueVs - 5)]]
        validSequences = [seq | seq <- sequences, isConsecutive seq] -- If there are 5 consecutive numbers
    in case validSequences of
        (seq:_) -> Just (valueToRank (head seq)) -- Return the highest card from the hand
        _       -> Nothing -- Else, return Nothing

-- Check if a list of integers is consecutive
isConsecutive :: [Int] -> Bool
isConsecutive xs = and $ zipWith (\a b -> a - b == 1) xs (tail xs)

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
    let counts = rankCounts hand
        threes = [rank | (rank, count) <- counts, count == 3] -- Look for any three of a kinds
        pairs  = [rank | (rank, count) <- counts, count == 2] -- Look for pairs
    in case (threes, pairs) of
        ((t:_), (p:_)) -> Just (FullHouse t p) -- If it's found, return FullHouse
        ((t:_), _)     -> -- If not pairs are found:
            -- Check for another three of a kind to act as a pair
            let remainingThrees = [rank | (rank, count) <- counts, count == 3, rank /= t]
            in case remainingThrees of
                (p:_) -> Just (FullHouse t p)
                _     -> Nothing
        _ -> Nothing -- if no full house, return Nothing

-- Same as isThreeOfAKind, except checking for 4 counts
isFourOfAKind :: [Card] -> Maybe HandRank
isFourOfAKind hand =
    let counts = rankCounts hand
        quads = [rank | (rank, count) <- counts, count == 4]
        remaining = [rank | (rank, count) <- counts, count /= 4]
    in case quads of
        (r:_) -> Just (FourOfAKind r (maximum remaining))
        _     -> Nothing

isStraightFlush :: [Card] -> Maybe HandRank
isStraightFlush hand =
    case isFlush hand of
        Just (Flush sortedRanks) ->
            -- Using isFlush, if there is a flush then check if there is a straight too
            let counts = suitCounts hand
                flushSuits = [suit | (suit, count) <- counts, count >= 5]
            in case flushSuits of
                (s:_) ->
                    -- Now that we have the flush suit, extract all cards of that suit
                    let flushCards = filter (\(su, _) -> su == s) hand
                    in case straightValue flushCards of -- We do not need to sort, since isFlush already has
                        Just highRank -> Just (StraightFlush highRank)
                        Nothing -> Nothing
                _ -> Nothing -- If there is no straight, return Nothing
        Nothing -> Nothing -- If there was no flush to begin with, return Nothing

isRoyalFlush :: [Card] -> Maybe HandRank
isRoyalFlush hand =
    case isStraightFlush hand of
        Just (StraightFlush rank) ->
            if rank == Ace then Just RoyalFlush else Nothing -- Check if its a straight flush, 
                                                             -- if it's highest card is an Ace, 
                                                             -- then it is a royal flush
        _ -> Nothing

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

                    