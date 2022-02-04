module Reactive(
    move_all_robot_reactive,
) where

import Board
import Random
import Data.Array
import Debug.Trace
import Data.List(unlines, unwords)
import Data.Foldable
import Moves
import Constants

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
        then cell == Empty || cell == Dirt || cell == Corral
    else cell == Empty || cell == Corral || cell == Dirt || cell == KidInCorral
    where 
        cell = board ! pos

bfs:: Matrix -> MatrixInt -> [Position] -> Cell-> MatrixInt
bfs board matrix [] cell = matrix
bfs board matrix positions cell = bfs board updated_bfs new_positions cell
    where
        x = head positions
        xs = tail positions
        valid_moves = filter (\v -> matrix ! v == inf && robot_can_move board v cell) $ adjacents x (n,m)
        value = matrix ! x + 1
        updated_bfs = matrix // [(pos, value) | pos <- valid_moves]
        new_positions = xs ++ valid_moves


calculate_matrix_BFS:: Matrix -> Position -> Cell -> MatrixInt
calculate_matrix_BFS board pos cell = bfs board matrix [pos] cell
    where 
        new = array ((0,0),(n,m)) [((i,j),inf) | i <- [0..n], j <- [0..m]]
        matrix = new //[(pos,0)]



closest_job:: Matrix -> MatrixInt -> Cell -> Position
closest_job board matrix cell = 
    if cell == Corral then 
        first_corral board
    else if null positions
        then (-1,-1)
    else 
        snd $ minimum positions
    where
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
    if robot_type == KidInRobotPassingCorral && first_corral board == position
        then (calculate_matrix_BFS board position Empty, Empty) 
    else if (not(kids_in_board board) && not(kids_in_robot_board board))
        then (calculate_matrix_BFS board position Dirt, Dirt)   
    else if not(kids_in_board board) && not(is_kid_in_robot $ board ! position)  
        then (calculate_matrix_BFS board position Empty, Empty) 
    else if is_robot_dirt robot_type
        then (calculate_matrix_BFS board position Dirt, Dirt)
    else if is_robot_kid robot_type
        then (calculate_matrix_BFS board position Kid, Kid)
    else if is_kid_in_robot robot_type
        then (calculate_matrix_BFS board position Corral, Corral)
    else (calculate_matrix_BFS board position Dirt, Dirt) 
    where
        percent = percent_clean board
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
    | otherwise = f KidInRobot
    where 
        e = board ! end
        b = board ! begin
        f = move_cell_change_final board begin end cell


change_cell_kid_in_robot::Matrix -> Position -> Position -> Cell -> Matrix
change_cell_kid_in_robot board begin end cell
    | e == Empty = f KidInRobot 
    | e == Dirt = f KidInRobotCleaning
    | e == Corral =  
        if first_corral board == end then
            f RobotAndKidInCorral
        else f KidInRobotPassingCorral
    | e == KidInCorral = board
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

change_cell_kid_in_robot_passing_corral::Matrix -> Position -> Position -> Matrix
change_cell_kid_in_robot_passing_corral board begin end 
    | e == Empty = f KidInRobot 
    | e == Dirt = f KidInRobotCleaning
    | e == Corral =
        if first_corral board == end then
            f RobotAndKidInCorral
        else f KidInRobotPassingCorral
    | e == KidInCorral = board
    | otherwise = board
    where 
        e = board ! end
        f = move_cell_change_final board begin end Corral

change_cell_robot_move::Matrix -> Position -> Position -> Matrix
change_cell_robot_move board begin end 
    | b == RobotDirt || b == RobotDirtCleaning = rd Empty
    | b == RobotDirtPassingCorral = rd Corral
    | b == RobotDirtPassingKidInCorral = rd KidInCorral
    | b == RobotKid || b == RobotKidCleaning = rk Empty
    | b == RobotKidPassingCorral = rk Corral
    | b == RobotKidPassingKidInCorral = rk KidInCorral
    | b == KidInRobot || b == KidInRobotCleaning = kr Empty
    | b == KidInRobotPassingCorral = 
        change_cell_kid_in_robot_passing_corral board begin end
    | otherwise = change_cell_robot_and_kid_in_corral board begin end
    where 
        rd = change_cell_robot_dirt board begin end 
        rk = change_cell_robot_kid board begin end 
        kr = change_cell_kid_in_robot board begin end
        b = board ! begin


move_robot::Matrix -> Position -> Matrix
move_robot board position =
    if closest /= (-1,-1) && matrix ! closest /= inf then 
        change_cell_robot_move board position movement
    else 
        board
    where
        temp = matrix_robot board position
        matrix = fst temp
        cell = snd temp
        closest = closest_job board matrix cell
        path = build_path matrix position closest
        movement:_ = path    


move_all_robot_reactive:: Matrix -> Bool -> Matrix
move_all_robot_reactive board flag =   
    if flag == True then foldl move_robot b r 
    else foldl move_robot b t
    where
        n = fst $ size board
        m = snd $ size board
        b = move_all_kid_in_robot board
        r = [(i,j) | i <- [0..n], j <- [0..m], is_robot $ b ! (i,j)]
        t = [(i,j) | i <- [0..n], j <- [0..m], is_robot $ b ! (i,j), not(cleaning_robots $ b ! (i,j))]

move_all_kid_in_robot::  Matrix -> Matrix
move_all_kid_in_robot board = foldl move_robot board k
    where
        n = fst $ size board
        m = snd $ size board
        k = [(i,j) | i <- [0..n], j <- [0..m], is_kid_in_robot_not_cleaning $ board ! (i,j)]
