{-# LANGUAGE BlockArguments #-}
module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Maybe (isJust, fromJust, isNothing, listToMaybe, catMaybes) -- POPRAWKA: Dodano catMaybes
import Data.List (find)
import CheckersLogic
import MinimaxBot (getBestMove) -- Import bota minimax

-- Rozszerzony stan gry
data GameState = GameState
    { board :: Board
    , currentPlayer :: PawnColor
    , selectedSquare :: Maybe Square
    , validMoves :: [Move] -- Lista dozwolonych ruchów
    , message :: String     -- Komunikaty dla gracza
    , gameOverState :: Maybe PawnColor -- Zwycięzca
    }

-- Konfiguracja okna
window :: Display
window = InWindow "Haskell Checkers" (650, 700) (100, 100)

backgroundColor :: Color
backgroundColor = white

squareSize :: Float
squareSize = 80.0

-- Rysowanie gry
render :: GameState -> Picture
render gs = pictures
    [ renderBoard
    , renderPieces (board gs)
    , renderSelection (selectedSquare gs)
    , renderValidMoveHighlights gs
    , renderMessage (message gs)
    , renderGameOver (gameOverState gs)
    ]

renderBoard :: Picture
renderBoard = pictures
    [ translate (xCoord c) (yCoord r) $ color (squareColor r c) $ rectangleSolid squareSize squareSize
    | r <- [0..7], c <- [0..7]
    ]
  where
    squareColor r c = if (r + c) `mod` 2 == 0 then light (light blue) else blue

renderPieces :: Board -> Picture
renderPieces brd = pictures [drawPiece p sq | (sq, p) <- brd]
  where
    drawPiece (Piece pColor pType) (r, c) =
        let pieceCol = if pColor == Red then red else black
            kingMarker = if pType == King then color white $ text "K" else blank
        in translate (xCoord c) (yCoord r) $ pictures
            [ color pieceCol $ thickCircle (squareSize * 0.35) 5
            , scale 0.3 0.3 kingMarker
            ]

renderSelection :: Maybe Square -> Picture
renderSelection Nothing = blank
renderSelection (Just (r, c)) =
    translate (xCoord c) (yCoord r) $ color yellow $ rectangleWire squareSize squareSize

renderValidMoveHighlights :: GameState -> Picture
renderValidMoveHighlights gs = case selectedSquare gs of
    Nothing -> blank
    Just s -> pictures
        [ translate (xCoord c) (yCoord r) $ color (light yellow) $ circle (squareSize * 0.1)
        | (start, (r, c), _) <- validMoves gs, start == s
        ]

renderMessage :: String -> Picture
renderMessage msg = translate (-300) 310 $ scale 0.2 0.2 $ color black $ text msg

renderGameOver :: Maybe PawnColor -> Picture
renderGameOver Nothing = blank
renderGameOver (Just winner) = pictures
    [ color (greyN 0.5) $ rectangleSolid 400 150
    , translate (-180) 20 $ scale 0.3 0.3 $ color white $ text "Game Over!"
    , translate (-150) (-40) $ scale 0.25 0.25 $ color white $ text (show winner ++ " wins!")
    ]

-- Transformacje koordynatów
xCoord :: Int -> Float
xCoord c = fromIntegral c * squareSize - (4 * squareSize) + squareSize / 2

yCoord :: Int -> Float
yCoord r = fromIntegral (7 - r) * squareSize - (4 * squareSize) + squareSize / 2

-- Inicjalizacja nowego stanu tury
newTurnState :: Board -> PawnColor -> GameState
newTurnState brd player =
    case checkGameOver brd player of
        Just winner -> GameState brd player Nothing [] ("Game Over! " ++ show winner ++ " wins.") (Just winner)
        Nothing ->
            let moves = allPossibleMoves brd player
            in GameState brd player Nothing moves (show player ++ "'s turn.") Nothing

-- Przetwarzanie ruchu
processMove :: GameState -> Move -> GameState
processMove gs move@(start, end, captured) =
    let newBoard = applyMove (board gs) move
        isJump = isJust captured
    in if isJump
       then -- Sprawdź, czy są kolejne bicia z pola 'end'
            let nextJumps = filter (\(s, _, cap) -> s == end && isJust cap) (allPossibleMoves newBoard (currentPlayer gs))
            in if not (null nextJumps)
               then -- Kontynuuj turę (multi-jump)
                    GameState newBoard (currentPlayer gs) (Just end) nextJumps "Multi-jump! You must jump again." Nothing
               else -- Zakończ turę
                    newTurnState newBoard (if currentPlayer gs == Red then Black else Red)
       else -- Zakończ turę po zwykłym ruchu
            newTurnState newBoard (if currentPlayer gs == Red then Black else Red)

-- Obsługa danych wejściowych
handleInput :: Event -> GameState -> GameState
handleInput _ gs | isJust (gameOverState gs) || currentPlayer gs == Black = gs -- Ignoruj input, gdy gra się skończyła lub tura bota
handleInput (EventKey (MouseButton LeftButton) Down _ (mx, my)) gs =
    let -- POPRAWKA: Prawidłowe obliczanie współrzędnych siatki na podstawie kliknięcia myszą
        c = floor ((mx + 4 * squareSize) / squareSize)
        r = 7 - floor ((my + 4 * squareSize) / squareSize)
        clickedSquare = (r, c)
    in if not (isValidSquare clickedSquare)
       then gs { selectedSquare = Nothing }
       else case selectedSquare gs of
           Nothing -> -- Wybór pionka
               if any (\(s, _, _) -> s == clickedSquare) (validMoves gs)
               then gs { selectedSquare = Just clickedSquare }
               else gs
           Just startSquare -> -- Wykonanie ruchu
               let maybeMove = find (\(s, e, _) -> s == startSquare && e == clickedSquare) (validMoves gs)
               in case maybeMove of
                   Just validMove -> processMove gs validMove
                   Nothing -> gs { selectedSquare = Nothing } -- Anuluj wybór, jeśli kliknięto nieprawidłowe pole

handleInput _ gs = gs

-- Logika aktualizacji (dla tury bota)
updateGame :: Float -> GameState -> GameState
updateGame _ gs
    | currentPlayer gs == Black && isNothing (gameOverState gs) =
        case getBestMove (board gs) Black 4 of -- Głębokość 4
            Just bestMove -> processMove gs bestMove
            Nothing -> gs -- Bot nie ma ruchów, co jest obsługiwane przez newTurnState
    | otherwise = gs


main :: IO ()
main =
    let initialState = newTurnState initialBoard Red
    in play window backgroundColor 1 initialState render handleInput updateGame