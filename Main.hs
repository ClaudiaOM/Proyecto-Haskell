import Board
import Moves
import Random
import Data.Array
import Data.Time.Clock
import Deductive
import Reactive
import Constants

execute_reactive::Matrix -> Int -> IO()
execute_reactive b turn = do   
    if mod turn change_environment == 0 then
        do 
            let r = remake_board b 
            putStr $ title_change_environment
            putStr $ board_to_string r
            execute_reactive r $ turn + 1 
    else 
        do
        b1 <- move_all_kid b 
        let board = move_all_robot_reactive b1 $ even turn
        putStr $ title_environment_turn ++ board_to_string b
        putStr $ title_robot_turn ++ board_to_string board
        if finished board
            then putStr $ title_finish ++ board_to_string board ++ title_complete
        else 
            execute_reactive board (turn + 1 )


execute_deductive::Matrix -> Int -> IO()
execute_deductive b turn = do   
    if mod turn change_environment == 0 then
        do 
            let r = remake_board b 
            putStr $ title_change_environment
            putStr $ board_to_string r
            execute_deductive r $ turn + 1 
    else 
        do
        b1 <- move_all_kid b 
        let board = move_all_robot_deductive b1 turn
        putStr $ title_environment_turn ++ board_to_string b
        putStr $ title_robot_turn ++ board_to_string board
        if finished board
            then putStr $ title_finish ++ board_to_string board ++ title_complete
        else 
            execute_deductive board (turn + 1 )



main_reactive::IO()
main_reactive = do 
    s1 <- seed
    s2 <- seed
    s3 <- seed
    let 
        x = array ((0,0),(n,m)) [((i,j), Empty) | i <- [0..n], j <- [0..m]]
        board = fill_board_reactive x (s1,s2,s3) (corral_heigth,corral_width,reactive_robots_dirt,
                                    reactive_robots_kids,obstacles,number_kids)     
    putStr $  board_to_string board
    putStr $ title_start
    execute_reactive board 1

main_deductive::IO()
main_deductive = do 
    s1 <- seed
    s2 <- seed
    s3 <- seed
    let 
        x = array ((0,0),(n,m)) [((i,j), Empty) | i <- [0..n], j <- [0..m]]
        board = fill_board_deductive x (s1,s2,s3) (corral_heigth,corral_width,deductive_robots
                        ,obstacles,number_kids)     
    putStr $ board_to_string board
    putStr $ title_start
    execute_deductive board 1

main:: IO()
main = do
    s1 <- seed
    s2 <- seed
    s3 <- seed
    let 
        x = array ((0,0),(n,m)) [((i,j), Empty) | i <- [0..n], j <- [0..m]]
        board1 = fill_board_deductive x (s1,s2,s3) (corral_heigth,corral_width,deductive_robots
                        ,obstacles,number_kids)  
        board2 = fill_board_reactive x (s1,s2,s3) (corral_heigth,corral_width,reactive_robots_dirt,
                                    reactive_robots_kids,obstacles,number_kids)  
    
    putStr title_start
    t <- fmap show getCurrentTime
    execute_reactive board1 1
    t1 <- fmap show getCurrentTime
    putStr title_finish
    putStr title_start
    
    t2 <- fmap show getCurrentTime
    execute_deductive board1 1
    t3 <- fmap show getCurrentTime
    putStr title_finish
    print "------------------"
    putStr t
    print "------------------"
    putStr t1
    print "------------------"
    putStr t2
    print "------------------"
    putStr t3



    
            