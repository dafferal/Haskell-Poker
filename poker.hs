{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid reverse" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Use null" #-}
{-# HLINT ignore "Use zipWith" #-}
import Data.List (sort, group, nub, subsequences)
import System.Random ( randomRIO )
import System.Exit (exitSuccess)

-- Defining the datatypes for Deck
data Suit 
    = Heart
    | Diamond
    | Spade
    | Club
    deriving (Show, Eq, Ord, Enum, Bounded)

data Rank 
    = Two
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
data Strategy 
    = RandomPlayer
    | PassivePlayer
    | AggressivePlayer
    deriving (Show, Eq)

data Player = Player {
    playerName :: String,
    hand       :: [Card],
    chips      :: Int,
    strategy   :: Strategy,
    isDealer   :: Bool, -- This bool value will be used to know where the big, 
                        -- and small blind is located in the list of players
    folded     :: Bool
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

-- Builds the deck using list comprehension
buildDeck :: Deck
buildDeck = [(suit, rank) | suit <- [Heart .. Club], rank <- [Two .. Ace]]

shuffleDeck :: Deck -> IO Deck
shuffleDeck [] = return [] -- Base case
shuffleDeck xs = do
    randIndex <- randomRIO (0, length xs - 1) -- Using the randomRIO monad to generate a random integer
    let
        remainingDeck = take randIndex xs ++ drop (randIndex + 1) xs -- Selecting random index within given Deck
    shuffledDeck <- shuffleDeck remainingDeck -- Recursively shuffle deck
    return (xs !! randIndex : shuffledDeck) 

-- Used to know what cards to deal
data DealType 
    = DealHoleCards
    | DealCommunityCards Int -- The integer holds the number of community cards to be dealt
    deriving (Show)

-- Used to deal community and hole cards to GameState
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
        (player { hand = hand player ++ cardsDealt } : updatedRestPlayers, finalDeck) 

-- Defining the HandRank data type
-- OnePair has the remaining cards to be used by isFullHouse
data HandRank 
    = HighCard [Rank] -- The ranks in descending order
    | OnePair Rank [Card] -- The rank pair and the remaining cards e.g. OnePair King [Ten, Six, Three]
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

-- Convert numeric value back to Rank for straights
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

-- High is just the sorted list of cards
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

-- Uses helperPair to find a one pair
isOnePair :: [Card] -> Maybe HandRank
isOnePair hand =
    let (pairs, remaining) = helperPair hand 2
    in case pairs of
        (x:_) -> Just (OnePair x remaining)
        _     -> Nothing

-- Uses helperPair to find two pairs
isTwoPair :: [Card] -> Maybe HandRank
isTwoPair hand =
    let (pairs, _) = helperPair hand 2
    in case pairs of
        (p1:p2:_) -> Just (TwoPair p1 p2)
        _         -> Nothing

-- Uses helperPair to find a three of a kind
isThreeOfAKind :: [Card] -> Maybe HandRank
isThreeOfAKind hand =
    let (triplets, _) = helperPair hand 3
    in case triplets of
        (t:_) -> Just (ThreeOfAKind t)
        _     -> Nothing

-- Processes a list of cards to see if the Ranks are consecutive
isStraight :: [Card] -> Maybe HandRank
isStraight hand =
    let cardRanks = ranks hand
        vs = map rankToValue cardRanks -- Convert the ranks to integers so I can process it
        vsWithAceLow = if Ace `elem` cardRanks then vs ++ [1] else vs -- Add 1 to the list for a low ace in the hand
        uniqueValues = nub vsWithAceLow -- Remove any repetitive ranks
        sortedValues = sort uniqueValues 
        finalValues = reverse sortedValues -- Sort the ranks from highest to lowest
        sequences = [take 5 (drop n finalValues) | n <- [0..(length finalValues - 5)]] -- I generate every sequence from the list of cards
        isConsecutive xs = and (zipWith (\a b -> a - b == 1) xs (tail xs)) 
        validSequences = [seq | seq <- sequences, isConsecutive seq] -- Check if they are consecutive (straights)
    in case validSequences of
        (seq:_) -> Just (Straight (valueToRank (head seq)))
        _       -> Nothing

-- Processes a list of cards to see if all the suits are the same
isFlush :: [Card] -> Maybe HandRank
isFlush hand =
    let flushSuits = [suit | (suit, count) <- suitCounts hand, count >= 5] -- Find any suits that are repeated 5 times
    in case flushSuits of
        (s:_) ->
            let flushCards = [rank | (suit, rank) <- hand, suit == s] 
                sortedRanks = sort flushCards
                finalRanks = reverse sortedRanks -- Get the ranks from the cards, then sort them from highest to lowest
            in Just (Flush finalRanks)
        _     -> Nothing -- If no flush, return Nothing

-- Uses isOnePair and isThreeOfAKind to look for a full house
isFullHouse :: [Card] -> Maybe HandRank
isFullHouse hand =
    case isOnePair hand of
        Nothing -> Nothing
        Just (OnePair a xs) -> -- Uses the remaining cards from isOnePair to look for a three of a kind
            case isThreeOfAKind xs of
                Nothing -> Nothing
                Just (ThreeOfAKind b) -> Just (FullHouse a b)

-- Uses helperPair to find a four of a kind
isFourOfAKind :: [Card] -> Maybe HandRank
isFourOfAKind hand =
    let (quads, _) = helperPair hand 4
    in case quads of
        (q:_) -> Just (FourOfAKind q)
        _     -> Nothing

-- Uses the isStraight and isFlush function to find a straight flush
isStraightFlush :: [Card] -> Maybe HandRank
isStraightFlush hand =
    case isFlush hand of
        Nothing -> Nothing
        Just (Flush _) -> -- If I find out the cards are a flush
            case isStraight hand of -- Check if its a straight too
                Nothing -> Nothing
                Just (Straight highRank) -> Just (StraightFlush highRank)

-- Use the isStraightFlush function to find a royal flush
isRoyalFlush :: [Card] -> Maybe HandRank
isRoyalFlush hand =
    case isStraightFlush hand of
        Nothing -> Nothing
        Just (StraightFlush rank) ->
            if rank == Ace then Just RoyalFlush else Nothing {-Check if its a straight flush, 
                                                              if it's highest card is an Ace, 
                                                              then it is a royal flush-} 

-- Goes through each hand ranking in order of strength,
-- If there is nothing found, its a high card
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

{- Since we have 7 cards, including both hole and community cards,
we have to look for every five card combo within these 7 cards,
and return the best hand using evaluteHand-}
getPlayerBestHand :: Player -> [Card] -> HandRank
getPlayerBestHand player community =
    let cards = hand player ++ community
        fiveCardCombos = filter (\x -> length x == 5) (subsequences cards)
        handRanks = map evaluateHand fiveCardCombos
    in maximum handRanks

{- Determine the winner (given a list of player names and hand ranks)
Returns a list of player names
If there is a tie, returns multiple names -}
determineWinner :: [(String, HandRank)] -> [String]
determineWinner playerHandRanks
    | null playerHandRanks = []
    | otherwise =
        let bestRank = maximum (map snd playerHandRanks)
            winners = [pName | (pName, rank) <- playerHandRanks, rank == bestRank]
        in winners

-- The implementation for Bet is implemented into Raise, as they are the same thing here
data Action = Fold
    | Call
    | Raise Int
    | Check
    deriving(Show)

-- Return how much the player has currently bet with a given name
-- If they aren't found, return 0
getPlayerBet :: String -> [(String, Int)] -> Int
getPlayerBet _ [] = 0
getPlayerBet pName ((name, bet):xs)
    | pName == name = bet
    | otherwise     = getPlayerBet pName xs

-- Update a players bet using their name
-- If they aren't found, add them to the bet list
updatePlayerBet :: String -> Int -> [(String, Int)] -> [(String, Int)]
updatePlayerBet pName newBet [] = [(pName, newBet)] 
updatePlayerBet pName newBet ((n,b):xs)
    | n == pName = (n, newBet) : xs
    | otherwise = (n, b) : updatePlayerBet pName newBet xs

-- Update a players chips using their name
-- If they aren't found, dont alter chips
updatePlayerChips :: String -> Int -> [Player] -> [Player]
updatePlayerChips _ _ [] = []
updatePlayerChips pName amountToCall (x:xs)
    | playerName x == pName = x { chips = chips x - amountToCall } : updatePlayerChips pName amountToCall xs
    | otherwise             = x : updatePlayerChips pName amountToCall xs

-- Find the highest bet between the players
-- If there aren't any, use 0
currentHighestBet :: [(String, Int)] -> Int
currentHighestBet betsList =
    if null betsList
        then 0
    else maximum [bet | (_, bet) <- betsList]

-- Used to check if any player needs to bet anymore
noMoreRaisesNeeded :: GameState -> Bool
noMoreRaisesNeeded state =
    case players state of
        [] -> True
        (x:xs) ->
            if getPlayerBet (playerName x) (bets state) /= currentHighestBet (bets state)
                then False
            else noMoreRaisesNeeded state { players = xs }

{- Used in bettingRound to return the action to be made by the
given player. It gets the player as an input, which then is used
to check for their strategy.
When raising/betting, I used the randomRIO monad to generate a 
number between 1 and the players chips.
When checking, it first checks if their current bet matches
the highest, if it doesn't, we have to call. -}
decideAction :: Player -> Int -> [(String, Int)] -> IO Action
decideAction player highestBet betsList = do
    let pChips = chips player
        currentBet = getPlayerBet (playerName player) betsList

    case strategy player of
        PassivePlayer -> do
            if highestBet > pChips
                then return Fold -- Cannot call since not enough chips
            else do
                -- The passive player only calls or folds, never raises
                actionChoice <- randomRIO (1 :: Int, 8)
                case actionChoice of
                    1 -> if currentBet == highestBet then return Check else return Call
                    2 -> if currentBet == highestBet then return Check else return Call
                    3 -> if currentBet == highestBet then return Check else return Call
                    4 -> if currentBet == highestBet then return Check else return Call
                    5 -> if currentBet == highestBet then return Check else return Call
                    _ -> return Fold

        AggressivePlayer -> do
            if highestBet > pChips
                then return Fold -- Cannot call since not enough chips
            else do
                -- The aggressive player bets and raises frequently
                actionChoice <- randomRIO (1 :: Int, 7)
                case actionChoice of
                    1 -> do
                        raiseAmount <- randomRIO (1 :: Int, pChips - highestBet)
                        return (Raise raiseAmount)
                    2 -> do
                        raiseAmount <- randomRIO (1 :: Int, pChips - highestBet)
                        return (Raise raiseAmount)
                    3 -> do
                        raiseAmount <- randomRIO (1 :: Int, pChips - highestBet)
                        return (Raise raiseAmount)
                    4 -> do
                        raiseAmount <- randomRIO (1 :: Int, pChips - highestBet)
                        return (Raise raiseAmount)
                    5 -> do
                        raiseAmount <- randomRIO (1 :: Int, pChips - highestBet)
                        return (Raise raiseAmount)
                    _ -> if currentBet == highestBet then return Check else return Call
        
        RandomPlayer -> do
            if highestBet > pChips
                then return Fold -- Cannot call since not enough chips
            else do
                -- The random player acts randomly
                actionChoice <- randomRIO (1 :: Int, 6)
                case actionChoice of
                    1 -> return Fold
                    2 -> return Fold
                    3 -> do
                        raiseAmount <- randomRIO (1 :: Int, pChips - highestBet)
                        return (Raise raiseAmount)
                    4 -> do
                        raiseAmount <- randomRIO (1 :: Int, pChips - highestBet)
                        return (Raise raiseAmount)
                    5 -> if currentBet == highestBet then return Check else return Call
                    _ -> if currentBet == highestBet then return Check else return Call


{- Used in the game loop for a singular betting round.
I first check if there are any raises needed, if not, continue.Applicative
Then I checked if the current list of players that are not folded is 1,
If it is, then there are no more players left to run a betting round, so it returns the GameState.
Once these checks have passed, it goes through the current player, calling decideAction to decide the action of the player.
It goes through each case of action, returing the updated state every time. -}
bettingRound :: GameState -> IO GameState
bettingRound state
    | noMoreRaisesNeeded state = return state
    | length (filter (not . folded) (players state)) == 1 = return state
    | otherwise = do
        let currentPlayers = players state
        let player = head currentPlayers
        let currentBets = bets state
        let currentName = playerName player
        let highestBet = currentHighestBet currentBets

        if folded player
            then return state
        else do
            action <- decideAction player highestBet currentBets
            case action of
                Fold -> do
                    let updatedPlayers = map (\x -> if playerName x == currentName then x { folded = True } else x) currentPlayers
                    return state { players = updatedPlayers }
                Check -> do
                    return state
                Call -> do
                    let newBets = updatePlayerBet currentName highestBet currentBets
                        newPlayers = updatePlayerChips currentName highestBet currentPlayers
                        newPot = pot state + highestBet
                    return state { bets = newBets, players = newPlayers, pot = newPot}

                Raise amt -> do
                    let totalCost = highestBet + amt
                        newBets = updatePlayerBet currentName totalCost currentBets
                        newPlayers = updatePlayerChips currentName totalCost currentPlayers
                        newPot = pot state + totalCost
                    return state { bets = newBets, players = newPlayers, pot = newPot }

{-Sets the positions for the dealer and blind players,
working like a circular table.
If there are two players in the game, the dealer has the small blind toom
and the other takes the big blind -}
setPositions :: GameState -> GameState
setPositions givenState =
    let numPlayers = length (players givenState)
        newDealer = (dealerPosition givenState + 1) `mod` numPlayers
        newSB = if numPlayers == 2 then newDealer else (newDealer + 1) `mod` numPlayers
        newBB = if numPlayers == 2 then (newDealer + 1) `mod` numPlayers else (newDealer + 2) `mod` numPlayers
    in givenState { dealerPosition = newDealer, smallBlindPosition = newSB, bigBlindPosition = newBB }

{- Using the isDealer Bool in the Player data type, 
we can find the index's for the small and big blinds.
This is used in game loop to fix the indexes in the GameState. -}
setPositionsFromDealer :: GameState -> GameState
setPositionsFromDealer givenState =
    let givenPlayers = players givenState
        numPlayers = length givenPlayers
        -- Find the index of the player who is the dealer in a given list of players
        dealerIndex = head [i | (x,i) <- zip givenPlayers [0..], isDealer x] 
                   
        -- Using that index, I calculate the small and big blind positions
        newSB = if numPlayers == 2 then dealerIndex else (dealerIndex + 1) `mod` numPlayers
        newBB = if numPlayers == 2 then (dealerIndex + 1) `mod` numPlayers else (dealerIndex + 2) `mod` numPlayers
    in givenState { dealerPosition = dealerIndex, smallBlindPosition = newSB, bigBlindPosition = newBB }

-- Deduct blinds from the big blind and small blind players
deductBlinds :: GameState -> GameState
deductBlinds givenState
    | null (players givenState) = givenState
    | otherwise =
        let givenPlayers = players givenState
            sbIndex = smallBlindPosition givenState
            bbIndex = bigBlindPosition givenState

            -- Deducts the blinds using the index of the small and big blinds in the players list
            updatedPlayers = zipWith (\x i -> if i == sbIndex 
                                                then x { chips = chips x - 1 }
                                              else if i == bbIndex
                                                then x { chips = chips x - 2 }
                                              else x) givenPlayers [0..]

            updatedPot = pot givenState + 3
            updatedBetsSB = updatePlayerBet (playerName (givenPlayers !! sbIndex)) 1 (bets givenState)
            updatesBetsBB = updatePlayerBet (playerName (givenPlayers !! bbIndex)) 2 updatedBetsSB
        in givenState { players = updatedPlayers, pot = updatedPot, bets = updatesBetsBB }

{- When the bettingRound starts, after the hole cards are dealt,
we need to make sure that it starts with the player after the big blind position-}
updatePlayersAfterHoleCardsState :: GameState -> GameState
updatePlayersAfterHoleCardsState givenState =
    let updatedPlayers = zipWith (\i x -> if i == dealerPosition givenState then x { isDealer = True } else x { isDealer = False }) [0..] (players givenState)
        startIndex = (bigBlindPosition givenState + 1) `mod` length updatedPlayers
    in givenState { players = drop startIndex updatedPlayers ++ take startIndex updatedPlayers }

{- After the flop, we have to start at the small blind position for betting -}
updatePlayersAfterFlop :: GameState -> GameState
updatePlayersAfterFlop givenState =
    let startIndex = smallBlindPosition givenState
        givenPlayers = players givenState
    in  givenState { players = drop startIndex givenPlayers ++ take startIndex givenPlayers }

{- The main function where the game runs
We start by checking the current round, we stop once we reach 100 rounds
We also stop when only one player remains in the game
We initalise the deck, set blind positions, deduct blinds, and start dealing hole communityCards
Once dealt, we run a bettingRound, after we place the flop and run another bettingRound
After that, we place the Turn, run another bettingRound, place the River and we go into the show down.
We then check for the winners using the determineWinners function, redistribute the pot,
Remove any players that have run out of chips, reinitalise the GameState and run the gameLoop again.-}
gameLoop :: GameState -> Int -> IO GameState
gameLoop currentState currentRound =
    if length (players currentState) == 1
        then do
            putStrLn ("And the winner(s) are: " ++ unwords [playerName player | player <- players currentState])
            return currentState
    else do
        putStrLn ("Round " ++ show currentRound ++ " starting" ++ "\n")
        shuffledDeck <- shuffleDeck buildDeck
        let shuffledDeckState = currentState {deck = shuffledDeck}
        let updatedState = setPositions shuffledDeckState
        let deductedBlindsState = deductBlinds updatedState
        let dealtHoleCardsState = dealCards DealHoleCards deductedBlindsState
        let playersAndCards = [(playerName x, hand x) | x <- players dealtHoleCardsState]
        let isFinalLoop = currentRound + 1 == 101
        putStrLn ("Players hand: " ++ show playersAndCards ++ "\n") 

        flopState <- bettingRound (setPositionsFromDealer (updatePlayersAfterHoleCardsState dealtHoleCardsState))
        let flopCardsState = dealCards (DealCommunityCards 3) flopState
        putStrLn ("Flop: " ++ show (communityCards flopCardsState) ++ "\n" )

        turnState <- bettingRound (setPositionsFromDealer (updatePlayersAfterFlop flopCardsState))
        let turnCardsState = dealCards (DealCommunityCards 1) turnState
        putStrLn ("Turn: " ++ show (communityCards turnCardsState) ++ "\n" )

        riverState <- bettingRound (setPositionsFromDealer (updatePlayersAfterFlop turnCardsState))
        let riverCardsState = dealCards (DealCommunityCards 1) riverState
        putStrLn ("River: " ++ show (communityCards riverCardsState) ++ "\n" )

        let notFolded = filter (not . folded) (players riverCardsState)
        let playerHands = [(playerName x, getPlayerBestHand x (communityCards riverCardsState)) | x <- notFolded]

        putStrLn ("Player(s) hand ranking: " ++ show playerHands ++ "\n" )

        let winners = case notFolded of
                [singlePlayer] ->
                    [playerName singlePlayer]
                _ ->
                    determineWinner playerHands

        let losers = [x | x <- players riverCardsState, playerName x `notElem` winners]

        -- Redistribute the pot to winners, making sure to check if there are any remainders
        -- If there are any remainders from division, add them back
        let updatedWinners =
                [ x { chips = chips x + (pot riverCardsState `div` length winners) + extraChip }
                | (x, index) <- zip (filter (\x -> playerName x `elem` winners) (players riverCardsState)) [0..]
                , let extraChip = if index < (pot riverCardsState `mod` length winners) then 1 else 0
                ]

        -- Combine winners and losers
        -- And remove any players that have run out of chips
        let updatedPlayers = filter (\x -> chips x > 0) (updatedWinners ++ losers)

        -- Reset the players hands, and if they're folded
        let resetPlayers = map (\x -> x { folded = False, hand =[] }) updatedPlayers

        -- Reset the pot, bets and community cards for the next loop
        let finalGameState = riverCardsState { players = resetPlayers, pot = 0, communityCards = []}

        putStrLn ("End of Round " ++ show currentRound ++ "\n")

        if isFinalLoop
            then do
                putStrLn "Game reached 100 rounds, no winners"
                return finalGameState
        else do
            gameLoop finalGameState (currentRound + 1)


{- Used to run a test scenario of 4 players with 1000 chips, 
and their respective strategies. -}
main :: IO()
main = do
    let initPlayers = [Player "Shams" [] 1000 RandomPlayer False False, 
            Player "Aayush" [] 1000 RandomPlayer False False, 
            Player "Devraj" [] 1000 RandomPlayer False False, 
            Player "Rishi" [] 1000 RandomPlayer False False]
            
    let initialState = GameState {
        players = initPlayers,
        deck = [],
        communityCards = [],
        pot = 0,
        bets = [],
        dealerPosition = -1,
        smallBlindPosition = -1,
        bigBlindPosition = -1
    }

    finalState <- gameLoop initialState 1
    exitSuccess