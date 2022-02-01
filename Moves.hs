module Moves(
    adjacents,
    is_robot,
    move_all_kid,
    show_bfs_matrix,
    adjacents_square
)
where 

import Board
import Random
import Data.Array
import Debug.Trace
import Data.List(unlines, unwords)
import Data.Foldable


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
    if valid_movement (xi,yi) (n,m)
        then
        if  board ! (xi,yi) == Obstacle
            then move_one_direction_obstacle board (move (xi,yi) dir) dir
        else if  board ! (xi,yi) == Empty
            then (True, (xi,yi))
        else (False, (xi,yi))
    else (False, (-1,-1))
    where
        n = fst $ size board
        m = snd $ size board


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
    board1 // [(last_pos, Kid)]
    where
        b = move_all_obstacles board (xf,yf) dir
        board1 = fst $ b
        last_pos = snd $ b


move_kid::Matrix -> Position -> IO Matrix
move_kid board (x,y)  = do 
    s <- seed
    let
        n = fst $ size board
        m = snd $ size board
        square = adjacents_square (x,y) (n,m)
        kids = length $ filter (\v -> board ! v == Kid ) square
        r = range_random s 4
        d = directions !! r
        op_dir = oposite_directions !! r
        new_pos = move (x,y) d
    if not(valid_movement new_pos (n,m)) 
        then return board
    else do
        let
            cell = board ! new_pos
            obs = move_one_direction_obstacle board new_pos d
            flag_obs = fst obs
            last_pos = snd obs
            res = if valid_movement new_pos (n,m) &&  not (occupied board new_pos) 
                    then move_kid_empty_cell board (x,y) new_pos kids s
                else if valid_movement new_pos (n,m) && cell == Obstacle && flag_obs == True
                    then move_kid_obstacle board (x,y) last_pos op_dir
                else 
                    board 
        return res
    


move_all_kid board = do
    let n = fst $ size board
        m = snd $ size board
        r = [(i,j) | i <- [0..n], j <- [0..m], board ! (i,j) == Kid]
    foldlM move_kid board r


show_bfs_matrix:: MatrixInt -> String
show_bfs_matrix board = unlines $ map row [0..a]
    where
        a = 7 --fst $ size board
        b = 7 --snd $ size board
        row i = unwords [show (board ! (i,j)) | j <- [0..b]] 