module Deductive where

import Constants
import Board
import Moves

import Data.Array
import Debug.Trace

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
        x = head positions
        xs = tail positions
        valid_moves = filter (\v -> matrix ! v == inf && robot_can_move board v cell) $ adjacents x (n,m)
        value = matrix ! x + 1
        updated_bfs = matrix // [(pos, value) | pos <- valid_moves]
        new_positions = xs ++ valid_moves


calculate_matrix_BFS:: Matrix -> Position -> Cell -> MatrixInt
calculate_matrix_BFS board pos cell =
    bfs board matrix [pos] cell
    where 
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
        positions = [(matrix!(i,j), (i,j)) | i <- [0..n], j <- [0..m], board ! (i,j) == cell ]


build_path matrix begin end = 
    if begin == end 
        then []
    else
        build_path matrix begin new_end ++ [end]
    where
        s = snd $ bounds matrix
        adj = adjacents end (n,m)
        current = matrix ! end
        new_end = head $ filter (\x -> matrix ! x == current - 1) adj


reactive_move::Matrix -> MatrixInt -> Position -> Cell -> (Position,Position)
reactive_move board matrix position cell = 
    if closest == (-1,-1) || matrix ! closest == inf then 
        ((-1,-1),(-1,-1))
    else 
        (closest,movement)
    where
        closest = closest_job board matrix cell
        path = build_path matrix position closest
        movement = head path 


objective_move:: Matrix -> MatrixInt -> Position -> (Position, Position)
objective_move board matrix pos = 
    if objective == (-1,-1) || matrix ! objective == inf then
        ((-1,-1),(-1,-1))
    else case b of
        Kid ->
            trace(show "Kid")
            trace(board_to_string board)
            trace(show objective)
            trace(show pos)
            movement
        Corral ->
            trace(show "Corral")
            trace(board_to_string board)
            trace(show objective)
            trace(show pos)
             movement
        Robot _ -> 
            trace(show "Robot")
            trace(board_to_string board)
            trace(show objective)
            trace(show pos)
            ((-1,-1),(-1,-1))
        _ -> 
            if null kids || matrix ! k == inf then 
                ((-1,-1),(-1,-1))
            else 
                trace(show "Else")
                trace(board_to_string board)
                trace(show objective)
                trace(show pos)
                trace(show kids)
                trace(show k)

                movement2
    where        
        (Robot (objective,cell,state)) = board ! pos    
        b = board ! objective    
        path = build_path matrix pos objective
        p1:_ = path
        movement = (objective, p1)    

        kids = filter (\v -> board ! v == Kid) $ adjacents_square objective (n,m)
        k:_ = kids
        path2 = build_path matrix pos k
        p2:_ = path2
        movement2 = (k,p2)


--MOVING CLEANING 
change_cell_state_moving_cleaning::Matrix -> Position -> Position -> Matrix
change_cell_state_moving_cleaning board begin end 
    | e == Empty = f (Robot (position,cell,Moving)) 
    | e == Dirt = f (Robot (position,cell,Cleaning)) 
    | e == Corral =  f (Robot (position,cell,PassingCorral)) 
    | e == KidInCorral = f (Robot (position,cell,PassingKidInCorral)) 
    | otherwise = f (Robot ((-1,-1),cell,CarryingKid)) 
    where
        (Robot (position,cell,state)) = board ! begin
        f = move_cell_change_final board begin end Empty 
        e = board ! end

--CARRYING KID   CARRYING KID CLEANING
change_cell_state_carrying_kid_cleaning::Matrix -> Position -> Position -> Matrix
change_cell_state_carrying_kid_cleaning board begin end 
    | e == Empty = f (Robot (position,cell,CarryingKid)) 
    | e == Dirt = f (Robot (position,cell,CarryingKidCleaning)) 
    | e == Corral =  f (Robot ((-1,-1),cell,KidAndRobotInCorral)) 
    | e == KidInCorral = f (Robot (position,cell,CarryingKidPassingKidInCorral)) 
    | otherwise = board --Imposible
    where
        (Robot (position,cell,state)) = board ! begin
        f = move_cell_change_final board  begin end Empty 
        e = board ! end

-- PASSING CORRAL
change_cell_state_passing_corral::Matrix -> Position -> Position -> Matrix
change_cell_state_passing_corral board begin end 
    | e == Empty = f (Robot (position,cell,Moving)) 
    | e == Dirt = f (Robot (position,cell,Cleaning)) 
    | e == Corral =  f (Robot (position,cell,PassingCorral)) 
    | e == KidInCorral = f (Robot (position,cell,PassingKidInCorral)) 
    | otherwise = f (Robot ((-1,-1),cell,CarryingKid)) --Imposible
    where
        (Robot (position,cell,state)) = board ! begin
        f = move_cell_change_final board  begin end Corral 
        e = board ! end

-- PASSING KID IN CORRAL
change_cell_state_passing_kid_corral::Matrix -> Position -> Position -> Matrix
change_cell_state_passing_kid_corral board begin end 
    | e == Empty = f (Robot (position,cell,Moving)) 
    | e == Dirt = f (Robot (position,cell,Cleaning)) 
    | e == Corral =  f (Robot (position,cell,PassingCorral)) 
    | e == KidInCorral = f (Robot (position,cell,PassingKidInCorral)) 
    | otherwise = f (Robot ((-1,-1),cell,CarryingKid)) 
    where
        (Robot (position,cell,state)) = board ! begin
        f = move_cell_change_final board  begin end KidInCorral 
        e = board ! end

-- CARRYING KID PASSING KID IN CORRAL
change_cell_state_carrying_kid_passing_kid_in_corral::Matrix -> Position -> Position -> Matrix
change_cell_state_carrying_kid_passing_kid_in_corral board begin end 
    | e == Empty = f (Robot (position,cell,CarryingKid)) 
    | e == Dirt = f (Robot (position,cell,CarryingKidCleaning)) 
    | e == Corral =  f (Robot ((-1,-1),cell,KidAndRobotInCorral)) 
    | e == KidInCorral = f (Robot (position,cell,CarryingKidPassingKidInCorral)) 
    | otherwise = f (Robot ((-1,-1),cell,CarryingKid)) --Imposible
    where
        (Robot (position,cell,state)) = board ! begin
        f = move_cell_change_final board  begin end KidInCorral 
        e = board ! end

-- KID AND ROBOT IN CORRAL
change_cell_state_kid_and_robot_in_corral::Matrix -> Position -> Position -> Matrix
change_cell_state_kid_and_robot_in_corral board begin end 
    | e == Empty = f (Robot ((-1,-1),cell,Moving)) 
    | e == Dirt = f (Robot ((-1,-1),cell,Cleaning)) 
    | e == Corral =  f (Robot ((-1,-1),cell,PassingCorral)) 
    | e == KidInCorral = f (Robot ((-1,-1),cell,PassingKidInCorral)) 
    | otherwise = f (Robot ((-1,-1),cell,CarryingKid))
    where
        (Robot (position,cell,state)) = board ! begin
        f = move_cell_change_final board begin end KidInCorral 
        e = board ! end

change_cell_robot_move::Matrix -> Position -> Position -> Matrix
change_cell_robot_move board begin end 
    | state == Moving || state == Cleaning = change_cell_state_moving_cleaning board begin end
    | state == CarryingKid || state == CarryingKidCleaning = change_cell_state_carrying_kid_cleaning board begin end
    | state == PassingCorral = change_cell_state_passing_corral board begin end
    | state == PassingKidInCorral = change_cell_state_passing_kid_corral board begin end
    | state == CarryingKidPassingKidInCorral= change_cell_state_carrying_kid_passing_kid_in_corral board begin end
    | state == KidAndRobotInCorral = change_cell_state_kid_and_robot_in_corral board begin end
    where        
        (Robot (_,_,state)) = board ! begin
        e = board ! end


move_robot::Matrix -> Position -> Matrix
move_robot board pos =
    if position == (-1,-1) then
        if state == CarryingKid then
            if free_corral board && rc_move /= (-1,-1) then
                trace(show "1")
                change_cell_robot_move rc_board pos rc_move 
            else 
                trace(show "12")
                board
        else if not(kids_in_board board) && not (kids_in_robot_board board) then
            if d_move /= (-1,-1) then
                trace(show "2")
                change_cell_robot_move d_board pos d_move
            else
                trace(show "3")
                board
        else if not(kids_in_board board) && kids_in_robot_board board && not(state == CarryingKid)
            then
                if dirt_in_board board && d_move /= (-1,-1) then
                     trace(show "4")
                    change_cell_robot_move d_board pos d_move
                else
                    trace(show "5")
                    board
        else          
            if k_move /= (-1,-1) then   
                trace(show "6")
                change_cell_robot_move k_board pos k_move  
            else 
                if dirt_in_board board && d_move /= (-1,-1) then
                    trace(show "61")
                    change_cell_robot_move d_board pos d_move
                else                    
                    board

    else
        if p == Corral then
            if c_move /= (-1,-1) then 
                trace(show "7")
                change_cell_robot_move c_board pos c_move
            else
                trace(show "71")
                board
        else if state == CarryingKid && rc_move /= (-1,-1) then 
            trace(show "8")
            trace(show pos)
            --trace(show rc_matrix)
            trace(show rc_goal)
            trace(show rc_move)
            trace(board_to_string rc_board)
            change_cell_robot_move rc_board pos rc_move 
        else if not(kids_in_board board) && not (kids_in_robot_board board) then
            if d_move /= (-1,-1) then 
                trace(show "10")  
                change_cell_robot_move d_board pos d_move
            else
                trace(show "11")
                board
        else if not(kids_in_board board) && kids_in_robot_board board && 
            not(state == CarryingKid) then
                if dirt_in_board board then 
                    if d_move /= (-1,-1) then
                        trace(show "12")
                        change_cell_robot_move d_board pos d_move
                    else
                        trace(show "121")
                        board
                else 
                    trace(show "13")
                    board
        else 
            if ok_move /= (-1,-1) then
                trace(show "14")
                change_cell_robot_move ok_board pos ok_move  
            else
                trace(show "15")
                board // [(pos, (Robot ((-1,-1),Kid,state)))]

    where
        (Robot (position,cell,state)) = board ! pos
        p = board ! position

        --REACTIVE KIDS
        k_matrix = calculate_matrix_BFS board pos Kid
        k = reactive_move board k_matrix pos Kid
        k_goal = fst k
        k_move = snd k
        k_board = board // [(pos, (Robot (k_goal,Kid,state)))]

        --REACTIVE DIRT
        d_matrix = calculate_matrix_BFS board pos Dirt
        d = reactive_move board d_matrix pos Dirt
        d_goal = fst d
        d_move = snd d
        d_board = board // [(pos, (Robot (d_goal,Dirt,state)))]
        
        --REACTIVE CORRAL
        rc_matrix = calculate_matrix_BFS board pos Corral
        rc = reactive_move board rc_matrix pos Corral
        rc_goal = fst rc
        rc_move = snd rc
        rc_board = board // [(pos, (Robot (rc_goal,Corral,state)))]

        -- OBJECTIVE 
        c_matrix = calculate_matrix_BFS board pos Corral
        c = objective_move board c_matrix pos 
        c_goal = fst c
        c_move = snd c
        c_board = board // [(pos, (Robot (c_goal,Corral,state)))]
            
        od_matrix = calculate_matrix_BFS board pos Dirt
        od = objective_move board od_matrix pos 
        od_goal = fst od
        od_move = snd od
        od_board = board // [(pos, (Robot (od_goal,Dirt,state)))]
        
        ok_matrix = calculate_matrix_BFS board pos Kid
        ok = objective_move board ok_matrix pos 
        ok_goal = fst ok
        ok_move = snd ok
        ok_board = board // [(pos, (Robot (ok_goal,Kid,state)))]




move_all_robot:: Matrix -> Bool -> Matrix
move_all_robot board flag =   
    if flag == True then
        foldl move_robot b r 
    else 
        foldl move_robot b t
    where        
        k = [(i,j) | i <- [0..n], j <- [0..m], is_kid_in_robot_not_cleaning $ board ! (i,j)]
        b = foldl move_robot board k
        r = [(i,j) | i <- [0..n], j <- [0..m], is_robot $ b ! (i,j)]
        t = [(i,j) | i <- [0..n], j <- [0..m], is_robot $ b ! (i,j), not(cleaning_robots $ b ! (i,j))]
