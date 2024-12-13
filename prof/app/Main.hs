module Main (main) where

    import Data.List (sort, group, nub, subsequences)
    import System.Random ( randomRIO )
    import Debug.Trace(trace, traceShowId, traceIO)

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
        | PassivePlayer
        | AggressivePlayer
            deriving (Show, Eq)

    data Player = Player {
        playerName :: String,
        hand       :: [Card],
        chips      :: Int,
        strategy   :: Strategy,
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
        deriving(Show)

    -- Return how much the player has currently bet with a given name
    -- If they aren't found, return 0
    getPlayerBet :: String -> [(String, Int)] -> Int
    getPlayerBet pName betsList =
        case lookup pName betsList of
            Nothing -> 0
            Just a  -> a

    -- Update a players bet using their name
    -- If they aren't found, don't change 
    updatePlayerBet :: String -> Int -> [(String, Int)] -> [(String, Int)]
    updatePlayerBet _ _ [] = []
    updatePlayerBet pName newBet ((n, b):xs)
    | n == pName = (n, newBet) : updatePlayerBet pName newBet xs
    | otherwise  = (n, b) : updatePlayerBet pName newBet xs

    -- Update a players chips using their name
    -- If they aren't found, dont alter chips
    updatePlayerChips :: String -> Int -> [Player] -> [Player]
    updatePlayerChips _ _ [] = []
    updatePlayerChips pName amountToCall (p:ps)
    | playerName p == pName = p { chips = chips p - amountToCall } : updatePlayerChips pName amountToCall ps
    | otherwise             = p : updatePlayerChips pName amountToCall ps

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
            (p:ps) ->
                if getPlayerBet (playerName p) (bets state) /= currentHighestBet (bets state)
                    then False
                else noMoreRaisesNeeded state { players = ps }

    decideAction :: Player -> Int -> [(String, Int)] -> IO Action
    decideAction player highestBet betsList = do
        let pChips = chips player
            currentBet = getPlayerBet (playerName player) betsList

        case strategy player of
            PassivePlayer -> do
                if (highestBet - currentBet) > pChips
                then
                    return Fold -- Cannot call since not enough chips
                else do
                        actionChoice <- randomRIO (1 :: Int, 3)
                        case actionChoice of
                        1 -> return Call
                        _ -> return Fold

            AggressivePlayer -> do
                if (highestBet - currentBet) > pChips
                then
                    return Fold -- Cannot call since not enough chips
                else do
                        actionChoice <- randomRIO (1 :: Int, 6)
                        case actionChoice of
                            1 -> return Call
                            2 -> do
                                raiseAmount <- randomRIO (1 :: Int, pChips)
                                return (Raise raiseAmount)
                            3 -> do
                                raiseAmount <- randomRIO (1 :: Int, pChips)
                                return (Raise raiseAmount)
                            4 -> do
                                raiseAmount <- randomRIO (1 :: Int, pChips)
                                return (Raise raiseAmount)
                            5 -> do
                                raiseAmount <- randomRIO (1 :: Int, pChips)
                                return (Raise raiseAmount)
                            _ -> return (Raise 1)

            RandomStrategy -> do
                if (highestBet - currentBet) > pChips
                then
                    return Fold -- Cannot call since not enough chips
                else do
                    actionChoice <- randomRIO (1 :: Int, 6)
                    case actionChoice of
                        1 -> return Call
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
            let currentPlayers = players state
                (player:remainingPlayers) = currentPlayers
                currentBets = bets state
                currentName = playerName player
                highestBet = currentHighestBet currentBets
            traceIO ("Current player: " ++ show currentPlayers)


            -- Skip the player if they have already folded
            if folded player
                then bettingRound state { players = remainingPlayers }
            else do
                let amountToCall = highestBet - getPlayerBet currentName currentBets

                -- Decide the player's action
                action <- decideAction player highestBet currentBets
                traceIO ("action of player: " ++ show action ++ " of player: " ++ show player ++ "\n")
                updatedState <- case action of
                    Fold -> do
                        traceIO ("player folded: " ++ show player ++ "\n")
                        let updatedPlayers = traceShowId $ map (\p -> if playerName p == currentName then p { folded = True } else p) currentPlayers
                        return state { players = updatedPlayers }

                    Call -> do
                        let newBets = updatePlayerBet currentName highestBet currentBets
                            newPlayers = updatePlayerChips currentName amountToCall currentPlayers
                        traceIO ("Current player after updatePlayerChips: " ++ show newPlayers ++ "\n")
                        return state { bets = newBets, players = newPlayers }

                    Raise amount -> do
                        let needed = highestBet - getPlayerBet currentName currentBets
                            totalCost = needed + amount
                            newBets = updatePlayerBet currentName (highestBet + amount) currentBets
                            newPlayers = updatePlayerChips currentName totalCost currentPlayers
                            newPot = pot state + totalCost
                        return state { bets = newBets, players = newPlayers, pot = newPot }
                traceIO ("updatedState after action " ++ show updatedState ++ "\n")

                -- Recursive call with updated state and remaining players
                let newPlayers = players updatedState
                bettingRound updatedState { players = tail newPlayers }


    -- Sets the positions for the dealer and blind players
    -- Works like a circular table
    setPositions :: GameState -> GameState
    setPositions givenState =
        let numPlayers = length (players givenState)
            newDealer = (dealerPosition givenState + 1) `mod` numPlayers
            newSB = (newDealer + 1) `mod` numPlayers
            newBB = (newDealer + 2) `mod` numPlayers
        in givenState { dealerPosition = newDealer, smallBlindPosition = newSB, bigBlindPosition = newBB }

    -- Updates the big and small blind players chips
    updatePlayerChipsByIndex :: [Player] -> Int -> Int -> Int -> [Player]
    updatePlayerChipsByIndex [] _ _ _ = [] -- The base case
    updatePlayerChipsByIndex (x:xs) sbIndex bbIndex currentIndex
        | currentIndex == sbIndex = (x { chips = chips x - 1 }) : updatePlayerChipsByIndex xs sbIndex bbIndex (currentIndex + 1)
        | currentIndex == bbIndex = (x { chips = chips x - 2 }) : updatePlayerChipsByIndex xs sbIndex bbIndex (currentIndex + 1)
        | otherwise               = x : updatePlayerChipsByIndex xs sbIndex bbIndex (currentIndex + 1)

    -- Deduct blinds from the big blind and small blind players
    deductBlinds :: GameState -> GameState
    deductBlinds givenState
        | null (players givenState) = givenState
        | otherwise =
            let ps = players givenState
                sbIndex = smallBlindPosition givenState
                bbIndex = bigBlindPosition givenState
                smallBlindPlayer = ps !! sbIndex
                bigBlindPlayer  = ps !! bbIndex

                sbChips = chips smallBlindPlayer - 1
                bbChips = chips bigBlindPlayer - 2

                updatedPlayers = updatePlayerChipsByIndex (players givenState) sbIndex bbIndex 0

                updatedPot = pot givenState + 3
                updatedBets = (playerName smallBlindPlayer, 1) : (playerName bigBlindPlayer, 2) : bets givenState
            in givenState { players = updatedPlayers, pot = updatedPot, bets = updatedBets }

    gameLoop :: GameState -> Int -> IO GameState
    gameLoop currentState currentRound =
        if currentRound > 100 || length (players currentState) == 1
            then return currentState
        else do
            putStrLn ("Round " ++ show currentRound ++ " starting" ++ "\n")
            shuffledDeck <- shuffleDeck buildDeck
            let shuffledDeckState = currentState {deck = shuffledDeck}
            let updatedState = setPositions shuffledDeckState
            let deductedBlindsState = deductBlinds updatedState
            let dealtHoleCardsState = dealCards DealHoleCards deductedBlindsState
            traceIO ("dealtHoleCardsState: " ++ show dealtHoleCardsState ++ "\n")


            preFlopState <- bettingRound dealtHoleCardsState
            let preFlopCardsState = dealCards (DealCommunityCards 3) preFlopState
            traceIO ("preFlopCardsState: " ++ show preFlopCardsState ++ "\n")


            flopState <- bettingRound preFlopCardsState
            let flopCardsState = dealCards (DealCommunityCards 1) flopState
            traceIO ("flopCardsState: " ++ show flopCardsState ++ "\n")


            riverState <- bettingRound flopCardsState
            let riverCardsState = dealCards (DealCommunityCards 1) riverState
            traceIO ("riverCardState: " ++ show riverCardsState ++ "\n")


            let notFolded = filter (not . folded) (players riverCardsState)
            let playerHands = [(playerName p, getPlayerBestHand p (communityCards riverCardsState)) | p <- notFolded]
            let winners = case notFolded of
                    [singlePlayer] ->
                        [playerName singlePlayer]
                    _ ->
                        determineWinner playerHands

            let losers = [p | p <- players riverCardsState, playerName p `notElem` winners]

            print playerHands
            print winners
            print losers
            print (players riverCardsState)

            -- Redistribute the pot to winners
            let updatedWinners = [(\ p
                                    -> p {chips = chips p
                                                    + (pot riverCardsState `div` length winners)})
                                    p |
                                    p <- players riverCardsState, playerName p `elem` winners]

            -- Combine winners and losers
            let updatedPlayers = filter (\p -> chips p < 0) (updatedWinners ++ losers)

            let resetPlayers = map (\p -> p { folded = False, hand =[] }) updatedPlayers
            let finalGameState = riverCardsState { players = resetPlayers, pot = 0, bets = [], communityCards = [],
            dealerPosition = 0, smallBlindPosition = -1, bigBlindPosition = -1}

            putStrLn $ "End of Round " ++ show currentRound
            putStrLn "Current Players:"
            putStrLn $ "Pot: " ++ show (pot finalGameState)
            gameLoop finalGameState (currentRound + 1)

main :: IO()
main = do
    let initPlayers = [Player "Shams" [] 1000 RandomStrategy False, Player "Idiot" [] 1000 AggressivePlayer False]
    let initialState = GameState {
        players = initPlayers,
        deck = [],
        communityCards = [],
        pot = 0,
        bets = [],
        dealerPosition = 0,
        smallBlindPosition = -1,
        bigBlindPosition = -1
    }

    finalState <- gameLoop initialState 1
    print finalState
