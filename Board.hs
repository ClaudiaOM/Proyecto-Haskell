module Board
( 
    Cell(..), 
    size,
    board_to_string,
    Matrix(..),
    occupied,
    fill_board,
    kids_in_board,
    fill_cell,
    finished
) where

import Data.Array
import Data.List(unlines, unwords)
import Random

data Cell = Empty | Dirt | Corral | Obstacle | KidInCorral |  
            Robot | Kid | KidInRobot | RobotKid | RobotDirt | RobotKidInCorral |
            RobotPassingCorral | RobotDirtPassingCorral | RobotKidPassingCorral
            deriving(Eq)

instance Show Cell where
    show Empty = "| |"
    show Dirt = "|D|"
    show Corral = "|-|"
    show Obstacle = "|O|"
    show KidInCorral = "|k|"
    show Robot = "|R|"
    show Kid = "|K|"
    show KidInRobot = "|r|"
    show RobotKid = "|1|"
    show RobotDirt = "|2|"
    show RobotKidInCorral = "|C|"
    show RobotPassingCorral = "|3|"
    show RobotDirtPassingCorral = "|4|"
    show RobotKidPassingCorral = "|5|"



type Matrix = Array (Int, Int) Cell
type Position = (Int, Int)
type Matrix_Size = (Int, Int)

size :: Matrix -> (Int, Int)
size b = snd $ bounds b

board_to_string :: Matrix -> String
board_to_string board = unlines $ map row [0..n]
    where
        n = fst $ size board
        m = snd $ size board
        row i = unwords [show (board ! (i,j)) | j <- [0..m]] 

occupied :: Matrix -> Position -> Bool
occupied matrix position = matrix ! position /= Empty


delete_sub_list :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
delete_sub_list a b = filter (`notElem` a) b 


positions_corral:: (Int, Int) -> (Int, Int) -> [(Int, Int)] 
-- Corral initial position, Corral size, positions for corral
positions_corral (a,b) (x,y) = [(i,j) | i <- [a..(a + x - 1)] , j <- [b..(b + y - 1)]]

fill_corral:: Matrix -> [(Int,Int)] -> Matrix
fill_corral board cor = board // [(p,Corral) | p <- cor]


fill_cell:: Matrix -> Int -> [(Int, Int)] -> Int -> Cell -> (Matrix, [(Int,Int)])
fill_cell board total positions seed cell=
    if total /= 0
    then
        fill_cell b (total - 1) new_positions (seed * total) cell
    else (board, positions) 
    where 
        rand = range_random seed $ length positions
        p = positions !! rand
        new_positions = delete_sub_list [p] positions
        b = board // [(p, cell)]
        


fill_board:: Matrix -> (Int, Int, Int) -> (Int, Int, Int, Int, Int, Int) -> Matrix  
fill_board board (cx, cy, r) (size_cor_x, size_cor_y, total_robot_dirt, total_robot_kid, total_obs, total_kid) = board4   
    where
        n = fst $ size board
        m = snd $ size board
        positions = [(i,j) | i <- [0..n], j <- [0..m]]
        corral_x = range_random cx $ n - size_cor_x
        corral_y = range_random cy $ m - size_cor_y
        corral = positions_corral (corral_x, corral_y) (size_cor_x, size_cor_y)
        boardt = fill_corral board corral
        positions_t = delete_sub_list corral positions
        temp0 = fill_cell boardt total_robot_dirt positions_t r RobotDirt
        positions_0 = snd temp0
        board0 = fst temp0
        temp1 = fill_cell board0 total_robot_kid positions_0 r RobotKid 
        positions_2 = snd temp1
        board2 = fst temp1
        temp2 = fill_cell board2 total_obs positions_2 (r * cx) Obstacle
        positions_3 = snd temp2
        board3 = fst temp2
        temp3 = fill_cell board3 total_kid positions_3 (r * cy) Kid
        board4 = fst temp3


kids_in_board:: Matrix -> Bool
kids_in_board board =  any (Kid == ) board 

kids_in_robot_board:: Matrix -> Bool
kids_in_robot_board board =  any (KidInRobot == ) board 

total_clean_cells:: Matrix -> Int
total_clean_cells board = length $ filter (\v -> board ! v == Empty) pos
                        where 
                            n = fst $ size board
                            m = snd $ size board
                            pos = [(i,j) | i<- [0..n], j<-[0..m]]

total_clean:: Matrix -> Bool
total_clean board = div (t * 100) (n * m) >= 60
    where
        n = fst $ size board
        m = snd $ size board
        t = total_clean_cells board

finished:: Matrix -> Bool
finished board = not (kids) && not (rob) && clean 
    where
        clean = total_clean board
        kids = kids_in_board board
        rob = kids_in_robot_board board