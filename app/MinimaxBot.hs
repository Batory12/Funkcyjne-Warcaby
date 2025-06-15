module MinimaxBot where

import CheckersCore
import Data.Maybe (isJust)

-- Typ dla wyniku algorytmu minimax
data MinimaxResult = MinimaxResult
    { bestMove :: Maybe Move
    , bestScore :: Float
    } deriving (Show)

-- Funkcja heurystyczna
heuristic :: Board -> PawnColor -> Float
heuristic board player = 
    let playerPieces = [piece | (sq, piece) <- board, colorOf piece == player]
        opponentPieces = [piece | (sq, piece) <- board, colorOf piece /= player]
        
        -- Punkty za pionki gracza
        playerScore = sum $ map pieceValue playerPieces
        -- Punkty za pionki przeciwnika (odejmowane)
        opponentScore = sum $ map pieceValue opponentPieces
        
        -- Bonus za bliskość do końca planszy dla zwykłych pionków
        proximityBonus = calculateProximityBonus board player
        
    in playerScore - opponentScore + proximityBonus
  where
    pieceValue (Piece _ Man) = 2.0
    pieceValue (Piece _ King) = 12.0  -- 2 za pionek + 10 za króla
    
    calculateProximityBonus :: Board -> PawnColor -> Float
    calculateProximityBonus brd plr =
        let playerMen = [(sq, piece) | (sq, piece) <- brd, colorOf piece == plr, isMan piece]
            targetRow = if plr == Red then 7 else 0
        in if null playerMen
           then 0.0
           else let distances = map (\((r, c), _) -> fromIntegral $ abs (r - targetRow)) playerMen
                    minDistance = minimum distances
                in -minDistance  -- -1 za każdą jednostkę odległości najbliższego pionka
    
    isMan (Piece _ Man) = True
    isMan (Piece _ King) = False

-- Główna funkcja minimax z alfa-beta pruning
minimax :: Board -> PawnColor -> Int -> Float -> Float -> Bool -> MinimaxResult
minimax board player depth alpha beta maximizing
    | depth == 0 = MinimaxResult Nothing (heuristic board player)
    | otherwise = 
        case gameOver board of
            Just winner -> 
                let score = if winner == player then 1000.0 else -1000.0
                in MinimaxResult Nothing score
            Nothing -> 
                let moves = allPossibleMoves board (if maximizing then player else opponent player)
                in if null moves
                   then MinimaxResult Nothing (if maximizing then -1000.0 else 1000.0)
                   else evaluateMoves moves alpha beta Nothing (if maximizing then -1001.0 else 1001.0)
  where
    opponent Red = Black
    opponent Black = Red
    
    evaluateMoves [] _ _ bestMove bestScoreVal = MinimaxResult bestMove bestScoreVal
    evaluateMoves (move:restMoves) currentAlpha currentBeta bestMove bestScoreVal =
        let newBoard = applyMove board move
            currentPlayer = if maximizing then opponent player else player
            result = minimax newBoard player (depth - 1) currentAlpha currentBeta (not maximizing)
            score = bestScore result
        in if maximizing
           then if score > bestScoreVal
                then let newBestMove = Just move
                         newBestScore = score
                         newAlpha = max currentAlpha score
                     in if newAlpha >= currentBeta
                        then MinimaxResult newBestMove newBestScore  -- Alpha-beta pruning
                        else evaluateMoves restMoves newAlpha currentBeta newBestMove newBestScore
                else evaluateMoves restMoves currentAlpha currentBeta bestMove bestScoreVal
           else if score < bestScoreVal
                then let newBestMove = Just move
                         newBestScore = score
                         newBeta = min currentBeta score
                     in if currentAlpha >= newBeta
                        then MinimaxResult newBestMove newBestScore  -- Alpha-beta pruning
                        else evaluateMoves restMoves currentAlpha newBeta newBestMove newBestScore
                else evaluateMoves restMoves currentAlpha currentBeta bestMove bestScoreVal

-- Funkcja pomocnicza do znajdowania najlepszego ruchu
getBestMove :: Board -> PawnColor -> Int -> Maybe Move
getBestMove board player depth = bestMove (minimax board player depth (-1001.0) 1001.0 True)

-- Funkcja do sprawdzenia czy ruch jest wieloskokiem
isMultiJump :: Board -> Move -> PawnColor -> Bool
isMultiJump board (start, end, captured) player =
    case captured of
        Nothing -> False
        Just _ -> 
            let newBoard = applyMove board (start, end, captured)
                nextJumps = [m | m@(s, _, Just _) <- possibleMoves newBoard player end]
            in not (null nextJumps)

-- Funkcja do wykonania pełnego wieloskoku (jeśli jest wymagany)
executeFullMultiJump :: Board -> Move -> PawnColor -> [Move]
executeFullMultiJump board move@(start, end, captured) player =
    if not (isMultiJump board move player)
    then [move]
    else let newBoard = applyMove board move
             nextJumps = [m | m@(s, _, Just _) <- possibleMoves newBoard player end]
         in if null nextJumps
            then [move]
            else case getBestMove newBoard player 1 of  -- Głębokość 1 dla następnego skoku
                Nothing -> [move]
                Just nextMove -> move : executeFullMultiJump newBoard nextMove player