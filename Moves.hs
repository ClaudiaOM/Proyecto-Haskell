module Moves(
    adjacents,
    closest_job,
    build_path,
    move_all_kid,
    move_all_robot,
    is_robot
)
where 

import Board
import Random
import Data.Array
import Debug.Trace
import Data.List(unlines, unwords)
import Data.Foldable


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


not_visited::Matrix -> MatrixInt -> Int -> Position -> Bool
not_visited board bfs inf pos = 
    bfs ! pos == inf && (cell == Empty || cell == Dirt || cell == Kid ) --TODO Ver cual Kid es
    where 
        cell = board ! pos

robot_can_move:: Matrix -> Position -> Cell -> Bool
robot_can_move board pos c =
    if c == Kid
        then cell == Empty || cell == Corral || cell == Dirt || cell == Kid || cell == KidInCorral
    else if c == Corral
        then cell == Empty || cell == Dirt || cell == Corral || cell == KidInCorral
    else cell == Empty || cell == Corral || cell == Dirt || cell == KidInCorral
    where 
        cell = board ! pos

bfs:: Matrix -> MatrixInt -> [Position] -> Cell-> MatrixInt
bfs board matrix [] cell = matrix
bfs board matrix positions cell = bfs board updated_bfs new_positions cell
    where
        n = fst $ size board
        m = snd $ size board
        inf = n * m
        x = head positions
        xs = tail positions
        valid_moves = filter (\v -> matrix ! v == inf && robot_can_move board v cell) $ adjacents x (n,m)
        value = matrix ! x + 1
        updated_bfs = matrix // [(pos, value) | pos <- valid_moves]
        new_positions = xs ++ valid_moves


calculate_matrix_BFS:: Matrix -> Position -> Cell -> MatrixInt
calculate_matrix_BFS board pos cell = bfs board matrix [pos] cell
    where 
        n = fst $ size board
        m = snd $ size board
        inf = n * m
        new = array ((0,0),(n,m)) [((i,j),inf) | i <- [0..n], j <- [0..m]]
        matrix = new //[(pos,0)]


closest_job:: Matrix -> MatrixInt -> Cell -> Position
closest_job board matrix cell = 
    if null positions
        then (-1,-1)
    else 
        snd $ minimum positions
    where
        n = fst $ size board
        m = snd $ size board
        positions = [(matrix!(i,j), (i,j)) | i <- [0..n], j <- [0..m], board ! (i,j) == cell ]


build_path matrix begin end = 
    if begin == end 
        then []
    else
        build_path matrix begin new_end ++ [end]
    where
        s = snd $ bounds matrix
        n = fst $ s
        m = snd $ s
        adj = adjacents end (n,m)
        current = matrix ! end
        new_end = head $ filter (\x -> matrix ! x == current - 1) adj


matrix_robot::Matrix -> Position -> (MatrixInt, Cell)
matrix_robot board position =
    if is_robot_dirt robot_type
        then (calculate_matrix_BFS board position Dirt, Dirt)
    else if is_robot_kid robot_type
        then (calculate_matrix_BFS board position Kid, Kid)
    else if is_kid_in_robot robot_type
        then (calculate_matrix_BFS board position Corral, Corral)
    else (calculate_matrix_BFS board position Kid, Kid) -- TODO ver que hacer con el robot que busca todo
    where
        robot_type = board ! position


change_cell_robot_dirt::Matrix -> Position -> Position -> Cell -> Matrix
change_cell_robot_dirt board begin end cell
    | e == Empty = f RobotDirt 
    | e == Dirt = f RobotDirtCleaning
    | e == Corral =  f RobotDirtPassingCorral
    | e == KidInCorral = f RobotDirtPassingKidInCorral
    | otherwise = board
    where 
        e = board ! end
        f = move_cell_change_final board begin end cell


change_cell_robot_kid::Matrix -> Position -> Position -> Cell -> Matrix
change_cell_robot_kid board begin end cell
    | e == Empty = f RobotKid 
    | e == Dirt = f RobotKidCleaning
    | e == Corral =  f RobotKidPassingCorral
    | e == KidInCorral = f RobotKidPassingKidInCorral
    | otherwise = if b == RobotKidPassingKidInCorral then board else f KidInRobot
    where 
        e = board ! end
        b = board ! begin
        f = move_cell_change_final board begin end cell


change_cell_kid_in_robot::Matrix -> Position -> Position -> Cell -> Matrix
change_cell_kid_in_robot board begin end cell
    | e == Empty = f KidInRobot 
    | e == Dirt = f KidInRobotCleaning
    | e == Corral =  f RobotAndKidInCorral
    | e == KidInCorral = f KidInRobotPassingKidInCorral
    | otherwise = board
    where 
        e = board ! end
        f = move_cell_change_final board begin end cell

change_cell_robot_and_kid_in_corral::Matrix -> Position -> Position -> Matrix
change_cell_robot_and_kid_in_corral board begin end 
    | e == Empty = f RobotKid 
    | e == Dirt = f RobotKidCleaning
    | e == Corral =  f RobotKidPassingCorral
    | e == KidInCorral = f RobotKidPassingKidInCorral
    | otherwise = f KidInRobot
    where 
        e = board ! end
        f = move_cell_change_final board begin end KidInCorral

change_cell_robot_move::Matrix -> Position -> Position -> Matrix
change_cell_robot_move board begin end 
    | b == RobotDirt || b == RobotDirtCleaning = rd Empty
    | b == RobotDirtPassingCorral = rd Corral
    | b == RobotDirtPassingKidInCorral = rd KidInCorral
    | b == RobotKid || b == RobotKidCleaning = rk Empty
    | b == RobotKidPassingCorral = rk Corral
    | b == RobotKidPassingKidInCorral = rk KidInCorral
    | b == KidInRobot || b == KidInRobotCleaning = kr Empty
    | b == KidInRobotPassingKidInCorral = kr KidInCorral
    | otherwise = change_cell_robot_and_kid_in_corral board begin end
    where 
        rd = change_cell_robot_dirt board begin end 
        rk = change_cell_robot_kid board begin end 
        kr = change_cell_kid_in_robot board begin end
        b = board ! begin


move_robot::Matrix -> Position -> Matrix
move_robot board position = 
    if closest /= (-1,-1) && matrix ! closest /= inf
        then 
        --trace(show position)        
        --trace(show closest)
        --trace(show_bfs_matrix matrix)
        change_cell_robot_move board position movement
    else 
        --trace(show position)
        board
    where
        n = fst $ size board
        m = snd $ size board
        inf = n * m
        temp = matrix_robot board position
        matrix = fst temp
        cell = snd temp
        closest = closest_job board matrix cell
        path = build_path matrix position closest
        movement = head path


move_all_robot:: Matrix -> Matrix
move_all_robot board = do 
    let n = fst $ size board
        m = snd $ size board
        r = [(i,j) | i <- [0..n], j <- [0..m], is_robot $ board ! (i,j)]
    foldl move_robot board r   


show_bfs_matrix:: MatrixInt -> String
show_bfs_matrix board = unlines $ map row [0..n]
    where
        n = 5
        m = 5
        row i = unwords [show (board ! (i,j)) | j <- [0..m]] 