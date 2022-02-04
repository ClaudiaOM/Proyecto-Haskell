module Board where

import Data.Array
import Data.List(unlines, unwords)
import Random
import Debug.Trace
import Constants


data State = Moving | Cleaning | CarryingKid | CarryingKidCleaning | PassingCorral 
            | PassingKidInCorral | CarryingKidPassingCorral | KidAndRobotInCorral
            deriving(Eq, Show)


data Cell = Empty | Dirt | Corral | Obstacle | KidInCorral | Kid |   --Cells
            RobotKid | RobotKidCleaning | RobotKidPassingCorral | RobotKidPassingKidInCorral |
            RobotDirt | RobotDirtCleaning | RobotDirtPassingCorral | RobotDirtPassingKidInCorral |
            KidInRobot | KidInRobotCleaning | KidInRobotPassingCorral | RobotAndKidInCorral |

            --DeductiveRobot
            Robot (Position, Cell, State)
            deriving(Eq)

instance Show Cell where
    show Empty = "| |"
    show Dirt = "|D|"
    show Corral = "|-|"
    show Obstacle = "|X|"
    show KidInCorral = "|k|"
    show Kid = "|K|"

    show RobotKid = "|R|"
    show RobotKidCleaning = "|R|"
    show RobotKidPassingCorral = "|R|"
    show RobotKidPassingKidInCorral = "|R|"

    show RobotDirt = "|\ESC[31mR\ESC[0m|"
    show RobotDirtCleaning = "|R|"
    show RobotDirtPassingCorral = "|R|"
    show RobotDirtPassingKidInCorral= "|R|"

    show KidInRobot = "|8|"
    show KidInRobotCleaning = "|8|"
    show KidInRobotPassingCorral = "|8|"

    show RobotAndKidInCorral = "|r|"

    show (Robot (_,_,CarryingKid)) = "|0|"
    show (Robot (_,_,KidAndRobotInCorral)) = "|1|"
    show (Robot (_,_,Moving)) = "|2|"
    show (Robot (_,_,Cleaning)) = "|3|"
    show (Robot (_,_,CarryingKidCleaning)) = "|4|"
    show (Robot (_,_,CarryingKidPassingCorral)) = "|5|"
    show (Robot (_,_,PassingCorral)) = "|6|"
    show (Robot (_,_,PassingKidInCorral)) = "|7|"
   -- show (Robot _) = "|R|"

type Matrix = Array (Int, Int) Cell
type Position = (Int, Int) 
type Matrix_Size = (Int, Int)
type MatrixInt = Array (Int, Int) Int

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

change_cell:: Matrix -> Cell -> Position -> Matrix
change_cell board cell position = board // [(position, cell)]

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
        

fill_board_reactive:: Matrix -> (Int, Int, Int) -> (Int, Int, Int, Int, Int, Int) -> Matrix  
fill_board_reactive board (cx, cy, r) (size_cor_x, size_cor_y, total_robot_dirt, 
                total_robot_kid, total_obs, total_kid) = board4   
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


fill_board_deductive:: Matrix -> (Int, Int, Int) -> (Int, Int, Int, Int, Int) -> Matrix  
fill_board_deductive board (cx, cy, r) (size_cor_x, size_cor_y, total_robot, total_obs, total_kid) 
                            = board4   
    where
        n = fst $ size board
        m = snd $ size board
        positions = [(i,j) | i <- [0..n], j <- [0..m]]
        corral_x = range_random cx $ n - size_cor_x
        corral_y = range_random cy $ m - size_cor_y
        corral = positions_corral (corral_x, corral_y) (size_cor_x, size_cor_y)
        boardt = fill_corral board corral
        positions_t = delete_sub_list corral positions
        temp0 = fill_cell boardt total_robot positions_t r (Robot ((-1,-1),Empty,Moving))
        positions_0 = snd temp0
        board0 = fst temp0
        temp2 = fill_cell board0 total_obs positions_0 (r * cx) Obstacle
        positions_3 = snd temp2
        board3 = fst temp2
        temp3 = fill_cell board3 total_kid positions_3 (r * cy) Kid
        board4 = fst temp3


remake_corral:: Matrix -> Matrix -> [(Int,Int)] -> Matrix
remake_corral new_board board [] = new_board 
remake_corral new_board board (x:xs) = remake_corral board1 board xs
    where 
        cell = board ! x
        board1 = new_board // [(x,cell)]


remake_cells:: Matrix -> Matrix -> Int -> [(Int,Int)] -> Matrix
remake_cells new_board board s [] = new_board 
remake_cells new_board board s (x:xs) = remake_cells board1 board (s + 1) xs 
    where 
        n = fst $ size board
        m = snd $ size board
        empty_positions =  [(i,j) | i <- [0..n], j <- [0..m], new_board ! (i,j) == Empty]
        cell = board ! x        
        board1 = fst $ fill_cell new_board 1 empty_positions s cell
 
remake_board:: Matrix -> Int -> Matrix
remake_board board s = board2
    where
        n = fst $ size board
        m = snd $ size board
        pieces = [(i,j) | i <- [0..n], j <- [0..m], board ! (i,j) /= Empty && not (is_corral $ board ! (i,j)) ]
        corral = [(i,j) | i <- [0..n], j <- [0..m], is_corral $ board ! (i,j)]

        board0 = array ((0,0),(n,m)) [((i,j), Empty) | i <- [0..n], j <- [0..m]]
        board1 = remake_corral board0 board corral 
        board2 = remake_cells board1 board s pieces

dirt_in_board:: Matrix -> Bool
dirt_in_board board = any (Dirt ==) board

free_corral::Matrix -> Bool
free_corral board = any (Corral ==) board

kids_in_board:: Matrix -> Bool
kids_in_board board =  any (Kid == ) board 

kids_in_robot_board:: Matrix -> Bool
kids_in_robot_board board =  any (is_kid_in_robot ) board 

total_clean_cells:: Matrix -> Int
total_clean_cells board = length $ filter (\v -> board ! v == Empty || board ! v == Dirt) pos
                        where 
                            n = fst $ size board
                            m = snd $ size board
                            pos = [(i,j) | i<- [0..n], j<-[0..m]]

total_dirt board =  length $ filter (\v -> board ! v == Dirt) pos
                        where 
                            n = fst $ size board
                            m = snd $ size board
                            pos = [(i,j) | i<- [0..n], j<-[0..m]]

total_clean:: Matrix -> Bool
total_clean board = 100 - div (x * 100) (t) >= 60
    where
        t = total_clean_cells board
        x = total_dirt board

percent_clean::Matrix -> Int
percent_clean board = 100 - div (x * 100) (t) 
    where
        t = total_clean_cells board
        x = total_dirt board

finished:: Matrix -> Bool
finished board = not (kids) && not (rob) && clean 
    where
        clean = total_clean board
        kids = kids_in_board board
        rob = kids_in_robot_board board

first_corral::Matrix ->  Position
first_corral board = 
    if null positions
        then (-1,-1)
    else 
        head positions
    where
        positions = [(i,j) | i <- [0..n], j <- [0..m], (is_free_corral $ board ! (i,j))  && board ! (i,j) /= KidInCorral]


is_robot_dirt::Cell -> Bool
is_robot_dirt c = c == RobotDirt || c == RobotDirtCleaning 
                || c == RobotDirtPassingCorral || c == RobotDirtPassingKidInCorral

is_robot_kid::Cell -> Bool
is_robot_kid c = c == RobotKid || c == RobotKidCleaning 
                || c == RobotKidPassingCorral || c == RobotKidPassingKidInCorral

is_kid_in_robot::Cell -> Bool
is_kid_in_robot (Robot (_ , _, state)) = state == CarryingKid || state == CarryingKidCleaning ||
                                          state == CarryingKidPassingCorral
is_kid_in_robot c = c == KidInRobot || c == KidInRobotCleaning || c == KidInRobotPassingCorral

is_kid_in_robot_not_cleaning::Cell -> Bool
is_kid_in_robot_not_cleaning (Robot (_ , _, state)) = state == CarryingKid || state == CarryingKidPassingCorral
is_kid_in_robot_not_cleaning c = c == KidInRobot || c == KidInRobotPassingCorral

is_robot::Cell -> Bool
is_robot (Robot _) = True
is_robot c = c == RobotAndKidInCorral || is_kid_in_robot c || is_robot_dirt c || is_robot_kid c

cleaning_robots::Cell -> Bool
cleaning_robots (Robot (_ , _, state)) = state == Cleaning || state == CarryingKidCleaning
cleaning_robots c = c == RobotDirtCleaning || c == RobotKidCleaning || c == KidInRobotCleaning 

is_corral::Cell -> Bool
is_corral (Robot (_ , _, s)) = s == PassingCorral || s == PassingKidInCorral ||
                               s == CarryingKidPassingCorral || s == KidAndRobotInCorral
is_corral c = c == Corral || c == KidInCorral || c == RobotAndKidInCorral || c == RobotDirtPassingCorral
                || c == RobotDirtPassingKidInCorral || c == RobotKidPassingCorral 
                || c == RobotKidPassingKidInCorral  || c == KidInRobotPassingCorral

is_free_corral::Cell -> Bool
is_free_corral (Robot (_ , _, s)) = s == PassingCorral ||
                               s == CarryingKidPassingCorral || s == KidAndRobotInCorral
is_free_corral c = c == Corral || c == RobotDirtPassingCorral
                || c == RobotKidPassingCorral || c == KidInRobotPassingCorral


is_carrying_kid::State -> Bool
is_carrying_kid c = c == CarryingKid || c == CarryingKidCleaning || c == CarryingKidPassingCorral               

move_cell_change_final:: Matrix -> Position -> Position -> Cell -> Cell -> Matrix
move_cell_change_final board (xi,yi) (xf,yf) celli cellf = board2
    where
        board1 =  board // [((xi,yi), celli )]
        board2 = board1 // [((xf,yf), cellf)]


