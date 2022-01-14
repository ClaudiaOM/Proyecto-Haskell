module Moves(
    move_cell,
    move_kid,
    adjacents_square,
    Direction (..),
    move_kid_empty_cell,
    move
) where 

import Board
import Random
import Data.Array
import Debug.Trace

type Position = (Int, Int)
type Matrix_Size = (Int, Int)
data Direction =  West | East | North | South | NorthWest | NorthEast | SouthWest | SouthEast

directions = [West, East, North, South]
all_directions = [West, East, North, South, NorthWest, NorthEast, SouthWest, SouthEast]

valid_movement :: Position -> (Int,Int) -> Bool
valid_movement (i,j) (n,m) = 0 <= i && i <= n && 0 <= j && j <= m 

move::Position -> Direction -> Position 
move (x,y) West = (x - 1, y)
move (x,y) East = (x + 1, y)
move (x,y) North = (x , y - 1)
move (x,y) South = (x , y + 1)
move (x,y) NorthWest = (x - 1, y - 1)
move (x,y) NorthEast = (x + 1, y - 1)
move (x,y) SouthWest = (x - 1, y + 1)
move (x,y) SouthEast = (x + 1, y + 1)


adjacents:: Position -> Matrix_Size -> [Position]
adjacents (x,y) (n,m) = [pos | pos <- map (move (x,y)) directions, valid_movement pos (n,m)]

adjacents_square:: Position -> Matrix_Size -> [Position]
adjacents_square (x,y) (n,m) = [pos | pos <- map (move (x,y)) all_directions, valid_movement pos (n,m)]


move_cell:: Matrix -> Position -> Position -> Cell -> Matrix
move_cell board (xi,yi) (xf,yf) cell = board2
    where
        piece = board ! (xi,yi) 
        board1 =  board // [((xi,yi), cell )]
        board2 = board1 // [((xf,yf), piece)]


dirt_in_square:: Matrix -> Position -> Int -> Int -> Matrix
dirt_in_square board (xi,yi) kids seed =
    if kids == 0 && 1 <= len
        then fst $ fill_cell board 1 s seed Dirt
    else if kids == 1 && 3 <= len
        then fst $ fill_cell board 3 s seed Dirt
    else if kids == 2 && 6 <= len
        then fst $ fill_cell board 6 s seed Dirt
    else 
        fst $ fill_cell board len s seed Dirt
    where 
        n = fst $ size board
        m = snd $ size board
        square = adjacents_square (xi,yi) (n,m)
        s = filter (\v -> board ! v == Empty) square
        len = length s

move_kid_empty_cell::Matrix -> Position -> Position -> Int -> Int -> Matrix
move_kid_empty_cell board (xi,yi) (xf,yf) kids seed = 
    dirt_in_square board1 (xi,yi) kids seed
    where
        board1 = move_cell board (xi,yi) (xf,yf) Empty


move_one_direction_obstacle::Matrix -> Position -> Direction -> Bool
move_one_direction_obstacle board (xi,yi) dir =
    if board ! (xi,yi) == Obstacle
        then move_one_direction_obstacle board (move (xi,yi) dir) dir
    else if  board ! (xi,yi) == Empty
        then True
    else False

move_all_obstacles:: Matrix -> Position -> Direction -> Matrix
move_all_obstacles board (xi,yi) dir = 
    if board ! (xi,yi) == Obstacle
        then move_all_obstacles board1 fin_pos dir
    else board1
    where
        fin_pos = move (xi,yi) dir
        board1 = move_cell board (xi,yi) fin_pos Obstacle

move_kid_obstacle::Matrix -> Position -> Position -> Direction -> Matrix
move_kid_obstacle board (xi,yi) (xf,yf) dir = 
    board1 // [((xf,yf), (Kid False))]
    where
        board1 = move_all_obstacles board (xf,yf) dir


move_kid::Matrix -> Position -> Int -> Matrix
move_kid board (x,y) seed = 
    if valid_movement new_pos (n,m) &&  not (occupied board new_pos) 
        then move_kid_empty_cell board (x,y) new_pos kids seed
    else if valid_movement new_pos (n,m) && cell == Obstacle
        then move_kid_obstacle board (x,y) new_pos d
    else 
        board 
    where
        n = fst $ size board
        m = snd $ size board
        square = adjacents_square (x,y) (n,m)
        kids = length $ filter (\v -> board ! v == (Kid False)) square
        d = directions !! (range_random seed 4)
        new_pos = move (x,y) d
        cell = board ! new_pos
        