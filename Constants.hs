
module Constants where

--Change Environment Variable (Amount of Turns)
change_environment::Int
change_environment = 10


--Board Size
--Width
n::Int
n = 5
--Height
m::Int
m = 5

corral_width::Int
corral_width = 3

corral_heigth::Int
corral_heigth = 3

number_kids::Int
number_kids = 9

obstacles::Int
obstacles = 0

--ROBOTS
reactive_robots_dirt::Int
reactive_robots_dirt = 1

--0 Value Not Permited
reactive_robots_kids::Int
reactive_robots_kids = 2

deductive_robots::Int
deductive_robots = 2


--Do not change
inf::Int
inf =  n * m

title_change_environment::String
title_change_environment = "-------------------------------\n"++
                            "ENVIRONMENT CHANGES \n" ++
                            "-------------------------------\n"
                            
title_robot_turn::String
title_robot_turn = "-------------------------------\n"++
                    "ROBOT TURN \n" ++
                    "-------------------------------\n"
                            
title_environment_turn::String
title_environment_turn = "-------------------------------\n"++
                         "ENVIRONMENT TURN \n" ++
                         "-------------------------------\n"

title_finish::String
title_finish = "-------------------------------\n"++
                "FINISH \n" ++
                "-------------------------------\n"

title_start::String
title_start = "-------------------------------\n"++
              "START \n" ++
              "-------------------------------\n"

title_complete::String
title_complete = "-------------------------------\n"++
                 "COMPLETE    \n" ++
                 "-------------------------------\n" 

title_percent::Int -> String
title_percent p = "-------------------------------\n"++
                 "Percent Clean     " ++ show p ++ 
                 "%\n-------------------------------\n" 

title_min_percent::Int -> String
title_min_percent p = "-------------------------------\n"++
                      "Min Percent Clean     " ++ show p ++ 
                      "%\n-------------------------------\n"

title_steps::Int -> String
title_steps p = "-------------------------------\n"++
                "Total Steps     " ++ show p ++ 
                "\n-------------------------------\n"
