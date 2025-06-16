module MinimaxBot where

import CheckersLogic
import Data.Maybe (isJust, fromJust, isNothing, catMaybes)
import Data.List (find)
data MinimaxResult = MinimaxResult
    { bestMove :: Maybe Move
    , bestScore :: Float
    } deriving (Show)

heuristic :: Board -> PawnColor -> Float
heuristic board player =
    let opponent = if player == Red then Black else Red
        playerPieces = filter (\(_, p) -> colorOf p == player) board
        opponentPieces = filter (\(_, p) -> colorOf p == opponent) board

        score piece = case piece of
            Piece _ Man -> 1.0
            Piece _ King -> 3.0 -- Król jest wart więcej

        playerScore = sum $ map (score . snd) playerPieces
        opponentScore = sum $ map (score . snd) opponentPieces

        -- Prosty bonus za pozycję dla pionków
        positionBonus (r, _) (Piece pColor pType)
            | pType == Man = if pColor == Red then fromIntegral r else fromIntegral (7 - r)
            | otherwise = 0 -- Można dodać bonus dla króli

        playerPosBonus = sum $ map (\(sq, p) -> positionBonus sq p) playerPieces
        
    in playerScore - opponentScore + 0.1 * playerPosBonus


minimax :: Board -> PawnColor -> Int -> Float -> Float -> Bool -> MinimaxResult
minimax board player depth alpha beta maximizingPlayer =
    case checkGameOver board (if maximizingPlayer then player else opponent) of
        Just winner -> MinimaxResult Nothing (if winner == player then 1000 else -1000)
        Nothing | depth == 0 -> MinimaxResult Nothing (heuristic board player)
        _ ->
            let moves = allPossibleMoves board (if maximizingPlayer then player else opponent)
                initialBest = if maximizingPlayer then -1/0 else 1/0
            in
                if null moves
                then MinimaxResult Nothing (if maximizingPlayer then -1000 else 1000)
                else go moves (MinimaxResult Nothing initialBest) alpha beta
  where
    opponent = if player == Red then Black else Red
    
    go [] bestResult _ _ = bestResult
    go (move:rest) bestResult currentAlpha currentBeta =
        let newBoard = applyMove board move
            (start, end, captured) = move
            -- Sprawdź, czy po biciu są kolejne bicia
            nextJumps = if isJust captured
                        then filter (\(_,_,cap) -> isJust cap) $ possibleMovesForPiece newBoard end
                        else []

            -- Jeśli są kolejne bicia, tura trwa dalej dla tego samego gracza
            (childScore, stillMaximizing) =
                if not (null nextJumps)
                then (bestScore $ minimax newBoard player (depth - 1) currentAlpha currentBeta maximizingPlayer, maximizingPlayer)
                else (bestScore $ minimax newBoard player (depth - 1) currentAlpha currentBeta (not maximizingPlayer), not maximizingPlayer)
                
            newBestResult =
                if maximizingPlayer
                then if childScore > bestScore bestResult
                     then MinimaxResult (Just move) childScore
                     else bestResult
                else if childScore < bestScore bestResult
                     then MinimaxResult (Just move) childScore
                     else bestResult
            
            (newAlpha, newBeta) = if maximizingPlayer
                                  then (max currentAlpha (bestScore newBestResult), currentBeta)
                                  else (currentAlpha, min currentBeta (bestScore newBestResult))
            
            -- Alfa-beta cięcie
            prune = newAlpha >= newBeta

        in if prune then newBestResult else go rest newBestResult newAlpha newBeta


getBestMove :: Board -> PawnColor -> Int -> Maybe Move
getBestMove board player depth = bestMove (minimax board player depth (-1/0) (1/0) True)