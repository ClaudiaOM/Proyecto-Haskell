module Moves(
    adjacents,
    calculate_matrix_BFS,
    move_kid,
    show_bfs_matrix,
    robot_can_move,
    bfs,
)
where 

import Board
import Random
import Data.Array
import Debug.Trace
import Data.List(unlines, unwords)

type Position = (Int, Int)
type Matrix_Size = (Int, Int)
type MatrixInt = Array (Int, Int) Int
data Direction =  West | East | North | South | NorthWest | NorthEast | SouthWest | SouthEast

directions = [West, East, North, South]
oposite_directions = [East, West, South, North]
all_directions = [West, East, North, South, NorthWest, NorthEast, SouthWest, SouthEast]

valid_movement :: Position -> (Int,Int) -> Bool
valid_movement (i,j) (n,m) = 0 <= i && i <= n && 0 <= j && j <= m 

move::Position -> Direction -> Position 
move (x,y) West = (x , y - 1)
move (x,y) East = (x , y + 1)
move (x,y) North = (x - 1, y)
move (x,y) South = (x + 1, y)
move (x,y) NorthWest = (x - 1, y - 1)
move (x,y) NorthEast = (x - 1, y + 1)
move (x,y) SouthWest = (x + 1, y - 1)
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

move_cell_change_final:: Matrix -> Position -> Position -> Cell -> Cell -> Matrix
move_cell_change_final board (xi,yi) (xf,yf) celli cellf = board2
    where
        board1 =  board // [((xi,yi), celli )]
        board2 = board1 // [((xf,yf), cellf)]


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
        square = adjacents_square (xi,yi) (n,m) ++ [(xi,yi)]
        s = filter (\v -> board ! v == Empty) square
        len = length s

move_kid_empty_cell::Matrix -> Position -> Position -> Int -> Int -> Matrix
move_kid_empty_cell board (xi,yi) (xf,yf) kids seed = 
    dirt_in_square board1 (xi,yi) kids seed
    where
        board1 = move_cell board (xi,yi) (xf,yf) Empty


move_one_direction_obstacle::Matrix -> Position -> Direction -> (Bool, (Int,Int))
move_one_direction_obstacle board (xi,yi) dir =
    if board ! (xi,yi) == Obstacle
        then move_one_direction_obstacle board (move (xi,yi) dir) dir
    else if  board ! (xi,yi) == Empty
        then (True, (xi,yi))
    else (False, (xi,yi))

move_all_obstacles:: Matrix -> Position -> Direction -> (Matrix, (Int, Int))
move_all_obstacles board (xi,yi) dir = 
    if board ! (xi,yi) == Empty && board ! fin_pos == Obstacle
        then move_all_obstacles board1 fin_pos dir
    else (board1, (xi,yi))
    where
        fin_pos = move (xi,yi) dir
        board1 = move_cell board (xi,yi) fin_pos Obstacle

move_kid_obstacle::Matrix -> Position -> Position -> Direction -> Matrix
move_kid_obstacle board (xi,yi) (xf,yf) dir = 
    board1 // [(last_pos, (Kid False))]
    where
        b = move_all_obstacles board (xf,yf) dir
        board1 = fst $ b
        last_pos = snd $ b


move_kid::Matrix -> Position -> Int -> Matrix
move_kid board (x,y) seed = 
    if valid_movement new_pos (n,m) &&  not (occupied board new_pos) 
        then move_kid_empty_cell board (x,y) new_pos kids seed
    else if valid_movement new_pos (n,m) && cell == Obstacle && flag_obs == True
        then move_kid_obstacle board (x,y) last_pos op_dir
    else 
        board 
    where
        n = fst $ size board
        m = snd $ size board
        square = adjacents_square (x,y) (n,m)
        kids = length $ filter (\v -> board ! v == (Kid False)) square
        r = range_random seed 4
        d = directions !! r
        op_dir = oposite_directions !! r
        new_pos = move (x,y) d
        cell = board ! new_pos
        obs = move_one_direction_obstacle board new_pos d
        flag_obs = fst obs
        last_pos = snd obs


not_visited::Matrix -> MatrixInt -> Int -> Position -> Bool
not_visited board bfs inf pos = 
    bfs ! pos == inf && (cell == Empty || cell == Dirt || cell == (Kid False) || cell == (Kid True)) --TODO Ver cual Kid es
    where 
        cell = board ! pos

robot_can_move:: Matrix -> Position -> Bool
robot_can_move board pos = 
    cell == Empty || cell == Dirt || cell == (Kid False) || cell == (Kid True) --TODO Ver cual Kid es
    where 
        cell = board ! pos

bfs:: Matrix -> MatrixInt -> [Position] -> MatrixInt
bfs board matrix [] = matrix
bfs board matrix positions = bfs board updated_bfs new_positions
    where
        n = fst $ size board
        m = snd $ size board
        inf = n * m
        x = head positions
        xs = tail positions
        valid_moves = filter (\v -> matrix ! v == inf && robot_can_move board v) $ adjacents x (n,m)
        value = matrix ! x + 1
        updated_bfs = matrix // [(pos, value) | pos <- valid_moves]
        new_positions = xs ++ valid_moves


calculate_matrix_BFS:: Matrix -> Position -> MatrixInt
calculate_matrix_BFS board pos = bfs board matrix [pos]
    where 
        n = fst $ size board
        m = snd $ size board
        inf = n * m
        new = array ((0,0),(n,m)) [((i,j),inf) | i <- [0..n], j <- [0..m]]
        matrix = new //[(pos,0)]

show_bfs_matrix:: MatrixInt -> String
show_bfs_matrix board = unlines $ map row [0..n]
    where
        n = 5
        m = 5
        row i = unwords [show (board ! (i,j)) | j <- [0..m]] 