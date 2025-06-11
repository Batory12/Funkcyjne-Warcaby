module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Checkers
import Data.Maybe (isJust, fromJust)



data GameState = GameState
    { board :: Board
    , currentPlayer :: PawnColor
    , selectedSquare :: Maybe Square
    } deriving (Eq, Show)


window :: Display
window = InWindow "Checkers" (600, 600) (100, 100)

backgroundColor :: Color
backgroundColor = white

squareSize :: Float
squareSize = 75

render :: GameState -> Picture
render (GameState brd _ sel) = pictures $
    [drawSquare (r, c) | r <- [0..7], c <- [0..7]] ++
    [drawPiece sq p | (sq, p) <- brd] ++
    [translate (xCoord c) (yCoord r) $ color red $ rectangleWire squareSize squareSize | Just (r, c) <- [sel]]
  where
    drawSquare (r, c) =
        let colorSquare = if (r + c) `mod` 2 == 0 then greyN 0.9 else greyN 0.4
        in translate (xCoord c) (yCoord r) $ color colorSquare $ rectangleSolid squareSize squareSize

    drawPiece (r, c) (Piece colorP typ) =
        let col = if colorP == Red then red else black
            kingMarker = if typ == King then scale 0.5 0.5 (text "K") else blank
        in translate (xCoord c) (yCoord r) $
             pictures [color col $ thickCircle 20 30, kingMarker]

    xCoord c = fromIntegral c * squareSize - 300 + squareSize / 2
    yCoord r = 300 - fromIntegral r * squareSize - squareSize / 2


handleInput :: Event -> GameState -> GameState
handleInput (EventKey (MouseButton LeftButton) Up _ (mx, my)) gs@(GameState brd plr sel) =
    let col = floor ((mx + 300) / squareSize)
        row = floor ((300 - my) / squareSize)
        clicked = (row, col)
    in if not (isValidSquare clicked) then gs
       else case sel of
           Nothing -> if isJust (getPiece brd clicked) && colorOf (fromJust (getPiece brd clicked)) == plr
                      then gs { selectedSquare = Just clicked }
                      else gs
           Just s -> let move = (s, clicked, if abs (fst clicked - fst s) == 2 then Just ((fst s + fst clicked) `div` 2, (snd s + snd clicked) `div` 2) else Nothing)
                     in if move `elem` allPossibleMoves brd plr
                        then let newBoard = applyMove brd move
                                 isJump = isJust (thrd move)
                                 nextJumps = [m | m@(start, _, Just _) <- possibleMoves newBoard plr clicked]
                             in if isJump && not (null nextJumps)
                                then GameState newBoard plr (Just clicked)
                                else GameState newBoard (nextPlayer plr) Nothing
                        else gs { selectedSquare = Nothing }
  where thrd (_, _, x) = x
        nextPlayer Red = Black
        nextPlayer Black = Red
handleInput _ gs = gs


main :: IO ()
main = do
    let initialState = GameState initialBoard Red Nothing
    play window backgroundColor 30 initialState render handleInput (\_ w -> w)