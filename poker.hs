{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid reverse" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Use null" #-}
import Data.List (sort, group, nub, subsequences)
import System.Random ( randomRIO )

-- Defining the datatypes for Deck
data Suit = Heart
    | Diamond
    | Spade
    | Club
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
    deriving (Show, Eq)

data Player = Player {
    playerName :: String,
    hand       :: [Card],
    chips      :: Int,
    isDealer   :: Bool,
    strategy   :: Strategy
} deriving (Show, Eq)

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
buildDeck = [(suit, rank) | suit <- [Heart .. Club], rank <- [Two .. Ace]]

shuffleDeck :: Deck -> IO Deck
shuffleDeck [] = return []
shuffleDeck xs = do
    randIndex <- randomRIO (0, length xs - 1)
    let 
        remainingDeck = take randIndex xs ++ drop (randIndex + 1) xs -- Selecting random index within given Deck
    shuffledDeck <- shuffleDeck remainingDeck -- Recursively shuffle deck
    return (xs !! randIndex : shuffledDeck)

-- Functions to deal hole and community cards
data DealType = DealHoleCards
    | DealCommunityCards Int -- Number of cards to be dealt
    deriving (Show)

dealCards :: DealType -> GameState -> GameState
dealCards dealType gameState =
    case dealType of
        DealHoleCards ->
            let 
                (updatedPlayers, newDeck) = dealToPlayers (players gameState) (deck gameState)
            in 
                gameState { players = updatedPlayers, deck = newDeck }
        DealCommunityCards numCards -> -- Deals cards depending on input
            let 
                (cardsDealt, newDeck) = splitAt numCards (deck gameState)
            in 
                gameState { communityCards = communityCards gameState ++ cardsDealt, deck = newDeck }

-- Helper function to deal two cards to each player
dealToPlayers :: [Player] -> Deck -> ([Player], Deck)
dealToPlayers [] deck = ([], deck)
dealToPlayers (player:restPlayers) deck =
    let
        (cardsDealt, deckAfterDealing) = splitAt 2 deck
        (updatedRestPlayers, finalDeck) = dealToPlayers restPlayers deckAfterDealing
    in
        (player { hand = hand player ++ cardsDealt } : updatedRestPlayers, finalDeck) -- Returns the updated players and deck

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

-- Count occurrences of each Suit, used for checking for flushes
suitCounts :: [Card] -> [(Suit, Int)]
suitCounts hand =
    let suitsList = suits hand
        sortedSuits = sort suitsList
        groupedSuits = group sortedSuits
        suitCountList = map (\group -> (head group, length group)) groupedSuits -- Count each suit
    in suitCountList

highCard :: [Card] -> HandRank
highCard hand =
    let cardRanks = ranks hand
        sortedRanks = sort cardRanks
        finalRanks = reverse sortedRanks
    in HighCard finalRanks

-- Helper function used to find pairs, triplets and quads
helperPair :: [Card] -> Int -> ([Rank], [Card])
helperPair hand a =
    let ranks = map snd hand
        sortedRanks = sort ranks
        groupedRanks = group sortedRanks
        rankCounts = map (\group -> (head group, length group)) groupedRanks
        pairs = [rank | (rank, count) <- rankCounts, count == a]
        remainingCards = filter (\(_, rank) -> rank `notElem` pairs) hand
    in (pairs, remainingCards)


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
        uniqueValues = nub vsWithAceLow
        sortedValues = sort uniqueValues
        finalValues = reverse sortedValues
        sequences = [take 5 (drop n finalValues) | n <- [0..(length finalValues - 5)]] -- We generate every sequence of cards we can, where it is consecutive
        isConsecutive xs = and (zipWith (\a b -> a - b == 1) xs (tail xs)) -- Check if they are consecutive (straights)
        validSequences = [seq | seq <- sequences, isConsecutive seq]
    in case validSequences of
        (seq:_) -> Just (Straight (valueToRank (head seq)))
        _       -> Nothing

isFlush :: [Card] -> Maybe HandRank
isFlush hand =
    let flushSuits = [suit | (suit, count) <- suitCounts hand, count >= 5] -- Find any suit that has 5 cards of the same suit
    in case flushSuits of
        (s:_) ->
            let flushCards = [rank | (suit, rank) <- hand, suit == s]
                sortedRanks = sort flushCards
                finalRanks = reverse sortedRanks
            in Just (Flush finalRanks)
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
        Nothing ->
            case isStraightFlush hand of
                Just hr -> hr
                Nothing ->
                    case isFourOfAKind hand of
                        Just hr -> hr
                        Nothing ->
                            case isFullHouse hand of
                                Just hr -> hr
                                Nothing ->
                                    case isFlush hand of
                                        Just hr -> hr
                                        Nothing ->
                                            case isStraight hand of
                                                Just hr -> hr
                                                Nothing ->
                                                    case isThreeOfAKind hand of
                                                        Just hr -> hr
                                                        Nothing ->
                                                            case isTwoPair hand of
                                                                Just hr -> hr
                                                                Nothing ->
                                                                    case isOnePair hand of
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

-- Helper functions for bettingRound

data Action = Fold
    | Call
    | Raise Int

-- Return how much the player has currently bet with a given name
-- If they aren't found, return 0
getPlayerBet :: String -> [(String, Int)] -> Int
getPlayerBet pName betsList =
    case lookup pName betsList of
        Nothing -> 0
        Just a -> a

-- Update a players bet using their name
-- If they aren't found, don't change 
updatePlayerBet :: String -> Int -> [(String, Int)] -> [(String, Int)]
updatePlayerBet pName newBet =
    map (\(n, b) -> if n == pName then (n, newBet) else (n, b))

-- Find the highest bet between the players
-- If there aren't any, use 0
currentHighestBet :: [(String, Int)] -> Int
currentHighestBet betsList =
    if null betsList
        then 0
    else maximum [bet | (_, bet) <- betsList]

-- Remove a player (used when they fold)
removePlayer :: String -> [Player] -> [Player]
removePlayer _ [] = []
removePlayer pName (p:ps)
    | playerName p == pName = removePlayer pName ps
    | otherwise             = p : removePlayer pName ps

-- Used to check if any player needs to bet anymore
noMoreRaisesNeeded :: GameState -> Bool
noMoreRaisesNeeded state =
    case players state of
        [] -> True
        (p:ps) ->
            if getPlayerBet (playerName p) (bets state) /= currentHighestBet (bets state)
            then False
            else noMoreRaisesNeeded state { players = ps }

decideAction :: Player -> Int -> [(String, Int)] -> IO Action
decideAction player highestBet betsList = do
    let pChips = chips player
        currentBet = getPlayerBet (playerName player) betsList

    case strategy player of
        PlaceholderStrategy ->
            return (if (highestBet - currentBet) <= pChips then Call else Fold)

        RandomStrategy -> do
            if (highestBet - currentBet) > pChips
               then
                 return Fold -- Cannot call since not enough chips
               else do
                 actionChoice <- randomRIO (1 :: Int, 6)
                 case actionChoice of
                     1 -> return Fold
                     2 -> return Call
                     3 -> return Call
                     4 -> return Call
                     5 -> do
                        raiseAmount <- randomRIO (1 :: Int, pChips)
                        return (Raise raiseAmount)
                     _ -> return (Raise 1)

bettingRound :: GameState -> IO GameState
bettingRound state
    | players state == [] = return state -- Base case: No players left to process
    | noMoreRaisesNeeded state = return state -- Base case: No more raises are needed
    | otherwise = do
        let highestBet = currentHighestBet (bets state)
        let (player:remainingPlayers) = players state

        -- Skip the player if they have already folded
        if player `notElem` players state
            then bettingRound state { players = remainingPlayers }
        else do
            let chipsLeft = chips player
                amountToCall = highestBet - getPlayerBet (playerName player) (bets state)

            -- Decide the player's action
            action <- decideAction player highestBet (bets state)
            updatedState <- case action of
                Fold -> do
                    return state { players = removePlayer (playerName player) (players state) }

                Call -> do
                    -- Update the player's bet and reduce their chips
                    let updatedBets = updatePlayerBet (playerName player) highestBet (bets state)
                        updatedPlayers = map (\p -> if playerName p == playerName player
                                                    then p { chips = chipsLeft - amountToCall }
                                                    else p) (players state)
                    return state { bets = updatedBets, players = updatedPlayers }

                Raise amount -> do
                    -- Ensure the player has enough chips to raise
                    if amount > chipsLeft
                        then return state
                        else do
                            let updatedBets = updatePlayerBet (playerName player) (highestBet + amount) (bets state)
                                updatedPlayers = map (\p -> if playerName p == playerName player
                                                            then p { chips = chipsLeft - (amountToCall + amount) }
                                                            else p) (players state)
                                updatedPot = pot state + amountToCall + amount
                            return state { bets = updatedBets, players = updatedPlayers, pot = updatedPot }

            -- Recursive call with updated state and remaining players
            bettingRound updatedState { players = remainingPlayers }
