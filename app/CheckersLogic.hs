module CheckersLogic where

import Data.Maybe (fromJust, isNothing, catMaybes, isJust)
-- Typy danych
type Square = (Int, Int)
data PawnColor = Red | Black deriving (Eq, Show)
data PieceType = Man | King deriving (Eq, Show)
data Piece = Piece PawnColor PieceType deriving (Eq, Show)
type Board = [(Square, Piece)]
type Move = (Square, Square, Maybe Square) -- (start, end, captured_square)

-- Funkcje pomocnicze
colorOf :: Piece -> PawnColor
colorOf (Piece c _) = c

isMan :: Piece -> Bool
isMan (Piece _ Man) = True
isMan _             = False

getPiece :: Board -> Square -> Maybe Piece
getPiece board square = lookup square board

isValidSquare :: Square -> Bool
isValidSquare (r, c) = r >= 0 && r < 8 && c >= 0 && c < 8

-- Plansza początkowa
initialBoard :: Board
initialBoard =
    [((r, c), Piece Red Man) | r <- [0..2], c <- [0..7], (r + c) `mod` 2 /= 0] ++
    [((r, c), Piece Black Man) | r <- [5..7], c <- [0..7], (r + c) `mod` 2 /= 0]

-- Logika ruchów
possibleMovesForPiece :: Board -> Square -> [Move]
possibleMovesForPiece board sq =
    case getPiece board sq of
        Nothing -> []
        Just piece@(Piece color pType) ->
            let
                forward = if color == Red then 1 else -1
                dirs = if pType == King
                       then [(1,1), (1,-1), (-1,1), (-1,-1)]
                       else [(forward, 1), (forward, -1)]

                simpleMoves = catMaybes -- POPRAWKA: Użycie catMaybes do odfiltrowania Nothing
                    [ let end = (fst sq + dr, snd sq + dc)
                      in if isValidSquare end && isNothing (getPiece board end)
                         then Just (sq, end, Nothing)
                         else Nothing
                    | (dr, dc) <- dirs
                    ]

                jumpMoves = catMaybes -- POPRAWKA: Użycie catMaybes do odfiltrowania Nothing
                    [ let capturedSq = (fst sq + dr, snd sq + dc)
                          end = (fst sq + 2*dr, snd sq + 2*dc)
                      in case getPiece board capturedSq of
                         Just capturedPiece | colorOf capturedPiece /= color && isValidSquare end && isNothing (getPiece board end) ->
                             Just (sq, end, Just capturedSq)
                         _ -> Nothing
                    | (dr, dc) <- dirs
                    ]
            in if not (null jumpMoves) then jumpMoves else simpleMoves

-- Zwraca wszystkie możliwe ruchy dla gracza, z priorytetem dla bić
allPossibleMoves :: Board -> PawnColor -> [Move]
allPossibleMoves board player =
    let allMoves = concatMap (possibleMovesForPiece board) [sq | (sq, p) <- board, colorOf p == player]
        jumps = filter (\(_, _, captured) -> isJust captured) allMoves
    in if not (null jumps) then jumps else allMoves

-- Aplikuje ruch na planszy
applyMove :: Board -> Move -> Board
applyMove board (start, end, captured) =
    let piece = fromJust (getPiece board start)
        -- Promocja na króla
        promotedPiece = if (fst end == 7 && colorOf piece == Red && isMan piece) || (fst end == 0 && colorOf piece == Black && isMan piece)
                        then Piece (colorOf piece) King
                        else piece
        -- Usuń pionek z pola startowego i ewentualnie zbity
        boardAfterRemoval = filter (\(sq, _) -> sq /= start && Just sq /= captured) board
    in (end, promotedPiece) : boardAfterRemoval

-- Sprawdza, czy gra się zakończyła
checkGameOver :: Board -> PawnColor -> Maybe PawnColor
checkGameOver board player =
    if null (allPossibleMoves board player)
    then Just (if player == Red then Black else Red) -- Gracz nie ma ruchów, przegrywa
    else
        let redPieces = any (\(_, p) -> colorOf p == Red) board
            blackPieces = any (\(_, p) -> colorOf p == Black) board
        in if not redPieces then Just Black
           else if not blackPieces then Just Red
           else Nothing