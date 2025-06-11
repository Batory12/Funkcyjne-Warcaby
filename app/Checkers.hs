module Checkers where

import Data.Maybe (isJust, fromJust, catMaybes, isNothing, maybe)
import Data.List (find)
import Text.Read (reads)

-- Types
type Square = (Int, Int)  -- (row, col)
data PawnColor = Red | Black deriving (Eq, Show)
data PieceType = Man | King deriving (Eq, Show)
data Piece = Piece PawnColor PieceType deriving (Eq, Show)
type Board = [(Square, Piece)]
type Move = (Square, Square, Maybe Square)  -- (start, end, captured square if jump)

-- Helper functions
readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
    [(x, "")] -> Just x
    _ -> Nothing

getPiece :: Board -> Square -> Maybe Piece
getPiece board square = lookup square board

isValidSquare :: Square -> Bool
isValidSquare (r, c) = r >= 0 && r <= 7 && c >= 0 && c <= 7 && (r + c) `mod` 2 == 1

initialBoard :: Board
initialBoard = 
    [((r, c), Piece Red Man) | r <- [0..2], c <- [0..7], (r + c) `mod` 2 == 1] ++
    [((r, c), Piece Black Man) | r <- [5..7], c <- [0..7], (r + c) `mod` 2 == 1]

-- Movement logic
possibleMoves :: Board -> PawnColor -> Square -> [Move]
possibleMoves board player square = 
    case getPiece board square of
        Nothing -> []
        Just (Piece color pieceType) -> if color /= player then [] else
            let (dr1, dr2) = if color == Red then (1, 2) else (-1, -2)
                directions = if pieceType == King then [(1, 1), (1, -1), (-1, 1), (-1, -1)] else [(dr1, 1), (dr1, -1)]
                simpleMoves = [(square, (r + dr, c + dc), Nothing) | (dr, dc) <- directions, 
                              let (r, c) = square, 
                              let newSquare = (r + dr, c + dc), 
                              isValidSquare newSquare, 
                              isNothing (getPiece board newSquare)]
                jumpMoves = [(square, (r + 2*dr, c + 2*dc), Just (r + dr, c + dc)) | 
                             (dr, dc) <- if pieceType == King then [(1, 1), (1, -1), (-1, 1), (-1, -1)] else [(dr2 `div` 2, 1), (dr2 `div` 2, -1)], 
                             let (r, c) = square, 
                             let midSquare = (r + dr, c + dc), 
                             let endSquare = (r + 2*dr, c + 2*dc), 
                             isValidSquare endSquare, 
                             isNothing (getPiece board endSquare), 
                             maybe False (\p -> colorOf p /= color) (getPiece board midSquare)]
            in jumpMoves ++ if null jumpMoves then simpleMoves else []

colorOf :: Piece -> PawnColor
colorOf (Piece c _) = c

allPossibleMoves :: Board -> PawnColor -> [Move]
allPossibleMoves board player = concat [possibleMoves board player square | (square, piece) <- board, colorOf piece == player]

applyMove :: Board -> Move -> Board
applyMove board (start, end, captured) = 
    let piece = fromJust (getPiece board start)
        newPiece = if (fst end == 7 && colorOf piece == Red) || (fst end == 0 && colorOf piece == Black) 
                   then Piece (colorOf piece) King 
                   else piece
        boardWithoutStart = filter ((/= start) . fst) board
        boardWithoutCaptured = maybe boardWithoutStart (\sq -> filter ((/= sq) . fst) boardWithoutStart) captured
    in (end, newPiece) : boardWithoutCaptured

gameOver :: Board -> Maybe PawnColor
gameOver board = 
    let redMoves = allPossibleMoves board Red
        blackMoves = allPossibleMoves board Black
    in if null redMoves && null blackMoves then Just (if length (filter ((== Red) . colorOf . snd) board) > length (filter ((== Black) . colorOf . snd) board) then Red else Black)
       else if null redMoves then Just Black
       else if null blackMoves then Just Red
       else Nothing

-- Pretty-prints the board with row and column labels
printBoard :: Board -> String
printBoard board = 
    let rows = [0..7]
        cols = [0..7]
        squareStr r c = case getPiece board (r, c) of
            Nothing -> if (r + c) `mod` 2 == 1 then "." else " "
            Just (Piece Red Man) -> "r"
            Just (Piece Black Man) -> "b"
            Just (Piece Red King) -> "R"
            Just (Piece Black King) -> "B"
        rowStr r = show r ++ " " ++ concat [squareStr r c ++ " " | c <- cols]
        header = "  " ++ concat [show c ++ " " | c <- cols]
    in unlines $ header : [rowStr r | r <- rows]

-- Plays the game interactively
playGame :: Board -> PawnColor -> IO ()
playGame board player = do
    putStrLn $ "\nCurrent board:\n" ++ printBoard board
    case gameOver board of
        Just winner -> putStrLn $ "Game Over! " ++ show winner ++ " wins!"
        Nothing -> do
            let moves = allPossibleMoves board player
                jumpMoves = [m | m@(start, end, Just _) <- moves]
                availableMoves = if null jumpMoves then moves else jumpMoves
            if null availableMoves
                then putStrLn $ show player ++ " has no moves! Game Over! " ++ show (if player == Red then Black else Red) ++ " wins!"
                else do
                    putStrLn $ show player ++ "'s turn. Enter move (row1 col1 row2 col2): "
                    input <- getLine
                    let parsed = case map readMaybe (words input) :: [Maybe Int] of
                            [Just r1, Just c1, Just r2, Just c2] -> Just [r1, c1, r2, c2]
                            _ -> Nothing
                    case parsed of
                        Just [r1, c1, r2, c2] -> do
                            let move = ((r1, c1), (r2, c2), if abs (r2 - r1) == 2 then Just ((r1 + r2) `div` 2, (c1 + c2) `div` 2) else Nothing)
                            if move `elem` availableMoves
                                then do
                                    let newBoard = applyMove board move
                                        isJump = isJust (thrd move)
                                        newPieceSquare = let (_, end, _) = move in end
                                        nextJumps = [m | m@(start, _, Just _) <- possibleMoves newBoard player newPieceSquare]
                                    if isJump && not (null nextJumps)
                                        then do
                                            putStrLn $ "\nYou landed at " ++ show newPieceSquare ++ " and can make another jump."
                                            playGameMultiJump newBoard player newPieceSquare
                                        else playGame newBoard (if player == Red then Black else Red)
                                else do
                                    putStrLn "Invalid move! Try again."
                                    playGame board player
                        _ -> do
                            putStrLn "Invalid input! Please enter four numbers (row1 col1 row2 col2)."
                            playGame board player
  where
    thrd (_, _, x) = x

-- Continues a player's turn for multi-jumps with enhanced prompts
playGameMultiJump :: Board -> PawnColor -> Square -> IO ()
playGameMultiJump board player square = do
    putStrLn $ "\nCurrent board:\n" ++ printBoard board
    let jumpMoves = [m | m@(start, _, Just _) <- possibleMoves board player square]
    if null jumpMoves
        then playGame board (if player == Red then Black else Red)
        else do
            let possibleDestinations = map (\(_, end, _) -> end) jumpMoves
            putStrLn $ show player ++ "'s turn: You must jump again from " ++ show square ++ ". Possible destinations: " ++ show possibleDestinations
            putStrLn "Enter destination (row2 col2): "
            input <- getLine
            let parsed = case map readMaybe (words input) :: [Maybe Int] of
                    [Just r2, Just c2] -> Just [r2, c2]
                    _ -> Nothing
            case parsed of
                Just [r2, c2] -> do
                    let move = (square, (r2, c2), if abs (r2 - fst square) == 2 then Just ((fst square + r2) `div` 2, (snd square + c2) `div` 2) else Nothing)
                    if move `elem` jumpMoves
                        then do
                            let newBoard = applyMove board move
                                nextJumps = [m | m@(start, _, Just _) <- possibleMoves newBoard player (r2, c2)]
                            if not (null nextJumps)
                                then playGameMultiJump newBoard player (r2, c2)
                                else playGame newBoard (if player == Red then Black else Red)
                        else do
                            putStrLn "Invalid jump! Try again."
                            playGameMultiJump board player square
                _ -> do
                    putStrLn "Invalid input! Please enter two numbers (row2 col2)."
                    playGameMultiJump board player square

-- Main function to start the game
main :: IO ()
main = do
    putStrLn "Welcome to Checkers!"
    playGame initialBoard Red